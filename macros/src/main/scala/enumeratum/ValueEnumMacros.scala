package enumeratum

import macrocompat.bundle

import scala.collection.immutable._
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

@bundle // macro-compat addition
class ValueEnumMacros(val c: blackbox.Context) {
  import c.universe._

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:Int implementations
    *
    * Note, requires the ValueEntryType to have a 'value' member that has a literal value
    */
  def findIntValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, Int]
  }

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:Long implementations
    *
    * Note, requires the ValueEntryType to have a 'value' member that has a literal value
    */
  def findLongValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, Long]
  }

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:Short implementations
    *
    * Note
    *
    *  - requires the ValueEntryType to have a 'value' member that has a literal value
    *  - the Short value should be a literal Int (do no need to cast .toShort).
    */
  def findShortValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, Short]
  }

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:String implementations
    *
    * Note
    *
    *  - requires the ValueEntryType to have a 'value' member that has a literal value
    */
  def findStringValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, String]
  }

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:Byte implementations
    *
    * Note
    *
    *  - requires the ValueEntryType to have a 'value' member that has a literal value
    */
  def findByteValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, Byte]
  }

  /**
    * Finds ValueEntryType-typed objects in scope that have literal value:Char implementations
    *
    * Note
    *
    *  - requires the ValueEntryType to have a 'value' member that has a literal value
    */
  def findCharValueEntriesImpl[ValueEntryType: c.WeakTypeTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    findValueEntriesImpl[ValueEntryType, Char]
  }

  /**
    * The method that does the heavy lifting.
    */
  private[this] def findValueEntriesImpl[ValueEntryType: c.WeakTypeTag, ValueType: ClassTag]: c.Expr[IndexedSeq[ValueEntryType]] = {
    import c.universe._
    val typeSymbol = weakTypeOf[ValueEntryType].typeSymbol
    EnumMacros.validateType(c)(typeSymbol)
    // Find the trees in the enclosing object that match the given ValueEntryType
    val subclassTrees = EnumMacros.enclosedSubClassTrees(c)(typeSymbol)
    // Find the parameters for the constructors of ValueEntryType
    val valueEntryTypeConstructorsParams = findConstructorParamsLists[ValueEntryType]
    // Identify the value:ValueType implementations for each of the trees we found and process them if required
    val treeWithVals = findValuesForSubclassTrees[ValueType](valueEntryTypeConstructorsParams, subclassTrees)
    // Make sure the processed found value implementations are unique
    ensureUnique[ValueType](treeWithVals)
    // Finish by building our Sequence
    val subclassSymbols = treeWithVals.map(_.tree.symbol)
    EnumMacros.buildSeqExpr[ValueEntryType](c)(subclassSymbols)
  }

  /**
    * Returns a list of TreeWithVal (tree with value of type ProcessedValueType) for the given trees and transformation
    *
    * Will abort compilation if not all the trees provided have a literal value member/constructor argument
    */
  private[this] def findValuesForSubclassTrees[ValueType: ClassTag](
      valueEntryCTorsParams: List[List[c.universe.Name]],
      memberTrees: Seq[c.universe.ModuleDef]): Seq[TreeWithVal[c.universe.ModuleDef, ValueType]] = {

    val treeWithValues = toTreeWithMaybeVals[ValueType](valueEntryCTorsParams, memberTrees)

    val (hasValueMember, lacksValueMember) = treeWithValues.partition(_.maybeValue.isDefined)

    if (lacksValueMember.nonEmpty) {
      val classTag = implicitly[ClassTag[ValueType]]
      val lacksValueMemberStr = lacksValueMember.map(_.tree.symbol).mkString(", ")

      c.abort(
        c.enclosingPosition,
        s"""
           |It looks like not all of the members have a literal/constant 'value:${classTag.runtimeClass.getSimpleName}' declaration, namely: $lacksValueMemberStr.
           |
           |This can happen if:
           |
           |- The aforementioned members have their `value` supplied by a variable, or otherwise defined as a method
           |
           |If none of the above apply to your case, it's likely you have discovered an issue with Enumeratum, so please file an issue :)
         """.stripMargin
      )
    }
    hasValueMember.collect {
      case TreeWithMaybeVal(tree, Some(v)) => TreeWithVal(tree, v)
    }
  }

  private[this] def evalExpression[ValueType](expr: Tree, declTree: c.universe.ModuleDef)(implicit classTag: ClassTag[ValueType]): ValueType = {
    try {
      // attempt evaluating the expression into the expected value type
      @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // false positive bug in WartRemover?
      val evaluated = c.eval(c.Expr[ValueType](c.untypecheck(c.typecheck(expr))))
      evaluated
    } catch {
      case t: Throwable =>
        // found a named argument with name "value", and an argument expression which cannot be evaluated into the right type
        c.abort(c.enclosingPosition, s"${declTree.symbol} has an arg $expr which could not be evaluated at compile time as a $classTag with error ${t.getMessage}.")
    }
  }

  /**
    * Looks through the given trees and tries to find the proper value declaration/constructor argument.
    *
    * Aborts compilation if the value declaration/constructor is of the wrong type,
    */
  private[this] def toTreeWithMaybeVals[ValueType](
      valueEntryCTorsParams: List[List[c.universe.Name]],
      memberTrees: Seq[c.universe.ModuleDef]
  )(implicit classTag: ClassTag[ValueType]): Seq[TreeWithMaybeVal[c.universe.ModuleDef, ValueType]] = {
    val valueTerm = TermName("value")
    // go through all the trees
    memberTrees.map { declTree =>
      val directMemberTrees = declTree.children.flatMap(_.children) // Things that are body-level, no lower
      val constructorTrees = {
        val immediate       = directMemberTrees // for 2.11+ this is enough
        val constructorName = termNames.CONSTRUCTOR
        // Sadly 2.10 has parent-class constructor calls nested inside a member..
        val method =
          directMemberTrees.collect { // for 2.10.x, we need to grab the body-level constructor method's trees
            case t @ DefDef(_, `constructorName`, _, _, _, _) => t.collect { case t => t }
          }.flatten
        immediate ++ method
      }.iterator

      val valuesFromMembers: Iterator[Option[ValueType]] = directMemberTrees.iterator.collect {
        case ValDef(_, termName, _, expr) if termName == valueTerm => Some(evalExpression(expr, declTree)(classTag))
      }

      // Sadly 2.10 has parent-class constructor calls nested inside a member..
      val valuesFromConstructors = constructorTrees.collect {
        // The tree has a method call
        case Apply(_, args) =>
          // Split the list into the arguments passed by position and the ones passed by name
          val (positional, named) = args.span {
            case AssignOrNamedArg(Ident(_), _) => false
            case _ => true
          }

          // Find a named argument that is "value"
          val maybeNamed: Option[ValueType] = named.collectFirst {
            // found a named argument with name "value", and an argument expression
            case AssignOrNamedArg(Ident(`valueTerm`), expr) => evalExpression(expr, declTree)(classTag)
          }

          // We only want the first such constructor argument
          val valueArgument: Option[ValueType] = maybeNamed match {
            case None =>
              // if a named argument wasn't found, try finding the parameter for "value" by position
              valueEntryCTorsParams.collectFirst {
                // Find non-empty constructor param lists
                case paramTermNames if paramTermNames.nonEmpty =>
                  (paramTermNames zip positional).collectFirst {
                    case (`valueTerm`, expr) => evalExpression(expr, declTree)(classTag)
                  }
              }.flatten
            case Some(solvedByName) => Some(solvedByName)
          }

          valueArgument
      }

      val values = valuesFromMembers ++ valuesFromConstructors
      val processedValue = values.collectFirst {
        case Some(v) => v
      }
      TreeWithMaybeVal(declTree, processedValue)
    }
  }

  /**
    * Returns a PartialFunction for turning symbols into names
    */
  def constructorsToParamNamesPF: PartialFunction[c.universe.Symbol, List[c.universe.Name]] = {
    case m if m.isConstructor => m.asMethod.paramLists.flatten.map(_.asTerm.name)
  }

  /**
    * Given a type, finds the constructor params lists for it
    */
  private[this] def findConstructorParamsLists[ValueEntryType: c.WeakTypeTag]: List[List[c.universe.Name]] = {
    val valueEntryTypeTpe        = implicitly[c.WeakTypeTag[ValueEntryType]].tpe
    val valueEntryTypeTpeMembers = valueEntryTypeTpe.members
    valueEntryTypeTpeMembers.collect(constructorsToParamNamesPF).toList
  }

  /**
    * Ensures that we have unique values for trees, aborting otherwise with a message indicating which trees have the same symbol
    */
  private[this] def ensureUnique[A](treeWithVals: Seq[TreeWithVal[c.universe.ModuleDef, A]]): Unit = {
    val membersWithValues = treeWithVals.map { treeWithVal =>
      treeWithVal.tree.symbol -> treeWithVal.value
    }
    val groupedByValue = membersWithValues.groupBy(_._2).mapValues(_.map(_._1))
    val (valuesWithOneSymbol, valuesWithMoreThanOneSymbol) =
      groupedByValue.partition(_._2.size <= 1)
    if (valuesWithOneSymbol.size != membersWithValues.toMap.keys.size) {
      val formattedString = valuesWithMoreThanOneSymbol.toSeq.reverse.foldLeft("") {
        case (acc, (k, v)) =>
          acc ++ s"""$k has members [ ${v.mkString(", ")} ]\n  """
      }
      c.abort(
        c.enclosingPosition,
        s"""
           |
           |  It does not look like you have unique values in your ValueEnum.
           |  Each of the following values correspond to more than one member:
           |
           |  $formattedString
           |  Please check to make sure members have unique values.
           |  """.stripMargin
      )
    }
  }

  // Helper case classes
  private[this] case class TreeWithMaybeVal[CTree, T](tree: CTree, maybeValue: Option[T])
  private[this] case class TreeWithVal[CTree, T](tree: CTree, value: T)

}
