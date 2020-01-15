package ui

import brainfuck.{BFManaged, BFToCPP, BFToScala, FlufflePuff, Ook}
import common.{EsoObj, Interpreter, Translator, Transpiler}
import deadfish.Deadfish
import emmental.Emmental
import fractran.{FracTran, FracTranpp}
import funge.{Befunge93, Befunge98}
import grass.Grass
import metatape.{BFToMetatape, Metatape}
import path.PATH
import pdoubleprime.PDP
import scala_run.ScalaRun
import slashes.Slashes
import snusp.{BFToSNUSP, SNUSP}
import thue.Thue
import unlambda.Unlambda
import whitespace.{WSAssembly, WhiteSpace, WhiteSpaceToScala}
import wierd.Wierd

import scala.collection.immutable

object EsoDefaults extends EsoObj{
  val defPointer: String = "Eso> "
  val defWelcome: String =
    """|Welcome to Eso, the functional esoteric language interpreter!
       |Type "help" for a list of commands.""".stripMargin
  
  val defBFLFile: String = "BFLangs.json"
  val defBindFile: String = "userBindings.json"
  val fileExtensionsVec: Vector[(String, String)] = Vector(
    ("slash", "///"),
    ("b93", "Befunge-93"),
    ("b98", "Befunge-98"),
    ("b", "BrainFuck"),
    ("df", "DeadFish"),
    ("emm", "Emmental"),
    ("ft", "FracTran"),
    ("ftp", "FracTran++"),
    ("grs", "Grass"),
    ("pdp", "P''"),
    ("path", "PATH"),
    ("snusp", "SNUSP"),
    ("scala", "Scala"),
    ("th", "Thue"),
    ("unl", "Unlambda"),
    ("ws", "WhiteSpace"),
    ("wd", "Wierd"),
    ("fl", "FlufflePuff"),
    ("ook", "Ook"),
    ("wsa", "WSAssembly"),
    ("mt", "Metatape"))
  val fileExtensionMap: immutable.HashMap[String, String] = mkMap(fileExtensionsVec)
  
  val defInterpVec: Vector[Interpreter] = Vector[Interpreter](
    BFManaged,
    WhiteSpace,
    FracTran,
    FracTranpp,
    Thue,
    PDP,
    Slashes,
    Deadfish,
    Emmental,
    Befunge93,
    Befunge98,
    Wierd,
    ScalaRun,
    Unlambda,
    SNUSP,
    Grass,
    PATH,
    Metatape)
  val defTransVec: Vector[Translator] = Vector[Translator](FlufflePuff, Ook, WSAssembly)
  val defGenVec: Vector[Transpiler] = Vector[Transpiler](BFToScala, BFToCPP, WhiteSpaceToScala, BFToSNUSP, BFToMetatape)
  val defBoolVec: Vector[(String, Boolean, String)] = Vector[(String, Boolean, String)](
    ("log", false, "toggle detailed console logging"),
    ("dyn", false, "resize tape as needed for BF interpreter to eliminate memory limitations"),
    ("fPtr", true, "toggle whether output for P'' programs starts at the read head going right or at the end of the tape going left"),
    ("sHead", true, "toggle whether the read head starts at the beginning of the initial tape or the right end of the tape for P''"),
    ("pNull", false, "toggle whether to print the null/empty character in the output of P'' programs"),
    ("indent", false, "toggle whether or not to neatly indent generated Scala code"),
    ("dfChar", true, "toggle whether or not to print Deadfish output as char values"),
    ("bfDiv", true, "toggle whether or not divison by 0 evaluates to 0 in Befunge-98"),
    ("bfRetCode", false, "toggle whether or not the Befunge-98 return code is displayed"),
    ("printNum", false, "print output as numerical values rather than characters"),
    ("time", false, "print program duration on completion"),
    ("appendInp", false, "append console input to the end of file input (useful for some self-interpreters)"),
    ("echoFileInp", true, "print file input to the console as it is used, makes it look as if the input was entered into the console directly"))
  val defNumVec: Vector[(String, Int, String)] = Vector[(String, Int, String)](
    ("bfOpt", 2, "BrainFuck interpreter selection: 0=base, 1=optimized, 2=compiled"),
    ("init", 40000, "initial tape size for interpreters with a data tape"),
    ("olen", -1, "maximum output length, useful for non-terminating programs, -1=infinite"),
    ("methSize", 1000, "maximum number of blocks in a generated method (for compiling interpreters"),
    ("mtCharWidth", 8, "bit width of input characters for Metatape"),
    ("fileEOF", 0, "character value to end file input strings with"))
  val defDesc: immutable.HashMap[String, String] = mkMap((defBoolVec ++ defNumVec).map{case (id, _, dc) => (id, dc)})
  
  val defInterpMap: immutable.HashMap[String, Interpreter] = mkMap(defInterpVec map (i => (i.name, i)))
  val defTransMap: immutable.HashMap[(String, String), Translator] = mkMap(defTransVec map (t => (t.id, t)))
  val defGenMap: immutable.HashMap[(String, String), Transpiler] = mkMap(defGenVec map (g => (g.id, g)))
  
  val nonPersistentHandlers: Vector[InterfaceHandler] = Vector(
    RunProgHandler,
    TranslateHandler,
    TranspileHandler,
    ShowSyntaxHandler,
    ListLangsHandler,
    ListVarsHandler,
    ListFileAssociationsHandler)
  
  val persistentHandlers: Vector[InterfaceHandler] = Vector(
    RunProgHandler,
    TranslateHandler,
    TranspileHandler,
    DefineBFLangHandler,
    LoadBFLangsHandler,
    SaveBFLangsHandler,
    ShowSyntaxHandler,
    ClearBindingsHandler,
    LoadBindingsHandler,
    SaveBindingsHandler,
    ListBindingsHandler,
    SetVarHandler,
    SetDefaultsHandler,
    ListLangsHandler,
    ListVarsHandler,
    ListFileAssociationsHandler,
    ExitHandler)
}
