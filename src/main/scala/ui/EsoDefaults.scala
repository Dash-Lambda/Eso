package ui

import alpl.ALPL
import brainfuck._
import common.{EsoObj, Interpreter, Translator, Transpiler}
import deadfish.Deadfish
import emmental.Emmental
import fractran.{FracTran, FracTranpp}
import funge.{Befunge93, Befunge98}
import glypho.{Glypho, GlyphoShorthand}
import grass.Grass
import lazybird.LazyBird
import lazyk._
import metatape.{BFToMetatape, Metatape}
import null_lang.NULL
import path.PATH
import pdoubleprime.PDP
import platts.Platts
import prelude.{BFToPrelude, Prelude}
import scala_run.ScalaRun
import slashes.Slashes
import snusp.{BFToSNUSP, SNUSP}
import thue.Thue
import unlambda.{LambdaToUnlambda, Unlambda}
import volatile.Volatile
import whitespace.{WSAssembly, WhiteSpace, WhiteSpaceToScala}
import wierd.Wierd
import wordlang.WordLang

import scala.collection.immutable

object EsoDefaults extends EsoObj{
  val defPointer: String = "Eso> "
  val defWelcome: String =
    s"""|Welcome to Eso (v${getClass.getPackage.getImplementationVersion}), the functional esoteric language interpreter!
        |Type "help" for a list of commands.""".stripMargin
  
  val defBFLFile: String = "BFLangs.json"
  val defBindFile: String = "userBindings.json"
  val defAssocFile: String = "fileAssoc.json"
  val fileExtensionsVec: Vector[(String, String)] = Vector(
    ("slash", "///"),
    ("b93", "Befunge-93"),
    ("b98", "Befunge-98"),
    ("b", "BrainFuck"),
    ("df", "Deadfish"),
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
    ("mt", "Metatape"),
    ("cpp", "C++"),
    ("pld", "Prelude"),
    ("nul", "NULL"),
    ("vol", "Volatile"),
    ("glo", "Glypho"),
    ("glos", "GlyphoShorthand"),
    ("plts", "Platts"),
    ("wl", "WordLang"),
    ("lazy", "LazyK"),
    ("alpl", "ALPL"),
    ("lzb", "LazyBird"))
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
    Metatape,
    Prelude,
    NULL,
    Volatile,
    Glypho,
    Platts,
    WordLang,
    LazyK,
    ALPL,
    LazyBird)
  val defTransVec: Vector[Translator] = Vector[Translator](FlufflePuff, Ook, WSAssembly, GlyphoShorthand, LazyKAnyToUnl, LazyKAnyToIota, LazyKAnyToCC, LazyKAnyToJot)
  val defGenVec: Vector[Transpiler] = Vector[Transpiler](BFToScala, BFToCPP, WhiteSpaceToScala, BFToSNUSP, BFToMetatape, BFToPrelude, BFToLazyK, LambdaToLazyKUnl, LambdaToUnlambda)
  
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
    ("echoFileInp", false, "print file input to the console as it is used, makes it look as if the input was entered into the console directly"),
    ("preludePar", false, "run Prelude voices in parallel, can speed up execution of some programs"),
    ("normLineBreaks", true, "normalize all line breaks to '\\n' when reading source files (for instance, '\\r\\n' => '\\n')"),
    ("debug", false, "toggle debug information for the interface and languages that support it"),
    ("cache", true, "cache initialized state of programs for faster repeated loading"))
  val defNumVec: Vector[(String, Int, String)] = Vector[(String, Int, String)](
    ("bfOpt", 2, "BrainFuck interpreter selection: 0=base, 1=optimized, 2=compiled"),
    ("init", 40000, "initial tape size for interpreters with a data tape"),
    ("olen", -1, "maximum output length, useful for non-terminating programs, -1=infinite"),
    ("methSize", 1000, "maximum number of blocks in a generated method (for compiling interpreters"),
    ("charWidth", 8, "bit width of input characters for languages that do bitwise I/O"),
    ("fileEOF", 0, "character value to end file input strings with"))
  val defDesc: immutable.HashMap[String, String] = mkMap((defBoolVec ++ defNumVec).map{case (id, _, dc) => (id, dc)})
  
  val defInterpMap: immutable.HashMap[String, Interpreter] = mkMap(defInterpVec map (i => (i.name, i)))
  val defTransMap: immutable.HashMap[(String, String), Translator] = mkMap(defTransVec map (t => (t.id, t)))
  val defGenMap: immutable.HashMap[(String, String), Transpiler] = mkMap(defGenVec map (g => (g.id, g)))
  
  val nonPersistentStartupLoaders: Vector[LoadHandler] = Vector(
    LoadBFLangsHandler(eio = EsoDummyInterface),
    LoadBindingsHandler(eio = EsoDummyInterface),
    LoadFileAssociationsHandler(eio = EsoDummyInterface))
  val nonPersistentHandlers: Vector[CommandHandler] = Vector(
    RunProgHandler(),
    TranslateHandler(),
    TranspileHandler(),
    ShowSyntaxHandler(),
    ListLangsHandler(),
    ListVarsHandler(),
    ListFileAssociationsHandler())
  
  val persistentStartupLoaders: Vector[LoadHandler] = Vector(
    LoadBFLangsHandler(eio = EsoDummyInterface),
    LoadBindingsHandler(eio = EsoDummyInterface),
    LoadFileAssociationsHandler(eio = EsoDummyInterface))
  val persistentHandlers: Vector[CommandHandler] = Vector(
    RunProgHandler(),
    TranslateHandler(),
    TranspileHandler(),
    DefineBFLangHandler(),
    LoadBFLangsHandler(),
    SaveBFLangsHandler(),
    ShowSyntaxHandler(),
    ClearBindingsHandler,
    ClearCacheHandler,
    LoadBindingsHandler(),
    SaveBindingsHandler(),
    ListBindingsHandler(),
    SetVarHandler(),
    SetDefaultsHandler,
    ListLangsHandler(),
    ListVarsHandler(),
    ListFileAssociationsHandler(),
    SaveFileAssociationsHandler(),
    LoadFileAssociationsHandler(),
    AddFileAssociationHandler,
    DropFileAssociationHandler,
    ClearFileAssociationsHandler,
    ExitHandler)
  
  def allHandlers(eio: EsoIOInterface = EsoConsoleInterface): Vector[CommandHandler] = Vector(
    RunProgHandler(eio),
    TranslateHandler(eio),
    TranspileHandler(eio),
    DefineBFLangHandler(eio),
    LoadBFLangsHandler(eio),
    SaveBFLangsHandler(eio),
    ShowSyntaxHandler(eio),
    ClearBindingsHandler,
    LoadBindingsHandler(eio),
    SaveBindingsHandler(),
    ListBindingsHandler(eio),
    SetVarHandler(eio),
    SetDefaultsHandler,
    ListLangsHandler(eio),
    ListVarsHandler(eio),
    ListFileAssociationsHandler(eio),
    ExitHandler)
}
