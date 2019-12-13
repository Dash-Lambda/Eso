package ui

import InterfaceHandlers._

object EsoExecutor {
  def apply(prs: EsoParsed)(state: EsoRunState): EsoState = prs match{
    case ParseFail =>
      println("Error: Invalid Command")
      state
    case EsoCmd(cmd, args) => cmd match{
      case "run" => runProg(state)(args)
      case "transpile" => genProg(state)(args)
      case "translate" => transProg(state)(args)
      case "defineBFLang" => defineBFLang(state)
      case "loadBFLangs" => loadBFLangs(state)(args)
      case "saveBFLangs" => saveBFLangs(state)(args)
      case "clrBindings" => clrBindings(state)
      case "listBindings" => listBindings(state)
      case "loadBindings" => loadBindings(state)(args)
      case "saveBindings" => saveBindings(state)(args)
      case "syntax" => syntax(state)(args)
      case "set" => setEnvVars(state)(args)
      case "defaults" => EsoRunState.default
      case "listLangs" => listLangs(state)
      case "listVars" => listEnvVars(state)
      case "help" => showHelp(state)
      case "exit" => exit()
      case _ => unknown(state)}}
}
