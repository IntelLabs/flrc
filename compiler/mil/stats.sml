(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


signature MIL_STATS =
sig
  datatype options = O of {id: string option}
  val layout : options -> Mil.t * Config.t -> Layout.t
  val program : Config.t * options * Mil.t * Out.t -> unit
end;

structure MilStats :> MIL_STATS =
struct

  val passname = "MilStats"

  structure M = Mil

  datatype options = O of {id: string option}

  datatype env = E of {config: Config.t, options: options}

  fun envMk (c, opts) = E {config = c, options = opts}

  datatype state = S of {
           globals   : int ref,
           funs      : int ref,
           blocks    : int ref,
           instrs    : int ref,
           calls     : int ref,
           tailcalls : int ref
  }

  fun stateMk () =
      S {globals = ref 0, funs = ref 0, blocks = ref 0, instrs = ref 0,
         calls = ref 0, tailcalls = ref 0}

  fun incr r = r := (!r) + 1

  fun incGlobals   (S {globals,   ...}) = incr globals
  fun incFuns      (S {funs,      ...}) = incr funs
  fun incBlocks    (S {blocks,    ...}) = incr blocks
  fun incInstrs    (S {instrs,    ...}) = incr instrs
  fun incCalls     (S {calls,     ...}) = incr calls
  fun incTailcalls (S {tailcalls, ...}) = incr tailcalls

  fun analyseInstruction (s, e, _) =
      let
        val () = incInstrs s
      in e
      end

  fun analyseInterProc (s, e, ip, r) =
      case (ip, r)
       of (M.IpCall _, M.RNormal _) => let val () = incCalls s in e end
        | (M.IpCall _, M.RTail   _) => let val () = incTailcalls s in e end
        | _                         => e

  fun analyseTransfer (s, e, l, t) =
      case t
       of M.TInterProc {callee, ret, ...} =>
          analyseInterProc (s, e, callee, ret)
        | _ => e

  fun analyseBlock (s, e, _ , _) =
      let
        val () = incBlocks s
      in e
      end

  fun analyseGlobal (s, e, _, g) =
      let
        val () = incGlobals s
        val () =
            case g
             of M.GCode _ => incFuns s
              | _ => ()
      in e
      end

  structure MA = MilAnalyseF(type state = state
                             type env = env
                             fun config (E {config, ...}) = config
                             val indent = 2
                             val externBind = NONE
                             val variableBind = NONE
                             val labelBind = NONE
                             val variableUse = NONE
                             val analyseJump = NONE
                             val analyseCut = NONE
                             val analyseConstant = NONE
                             val analyseInstruction = SOME analyseInstruction
                             val analyseTransfer = SOME analyseTransfer
                             val analyseBlock = SOME analyseBlock
                             val analyseGlobal = SOME analyseGlobal)

  local
    open Layout
  in

  fun layoutStats (O {id, ...}, s) =
      let
        val S {globals, funs, blocks, instrs, calls, tailcalls} = s
        fun doOne (s, r) = seq [str ("  Number of " ^ s), Int.layout (!r)]
        val l = align [doOne ("globals:   ", globals),
                       doOne ("funs:      ", funs),
                       doOne ("blocks:    ", blocks),
                       doOne ("instrs:    ", instrs),
                       doOne ("calls:     ", calls),
                       doOne ("tailcalls: ", tailcalls)]
        val l =
            case id
             of NONE => l
              | SOME id =>
                align [str ("---------- Stats for: " ^ id),
                       l,
                       str ("---------- End stats for: " ^ id ^ "\n")]
      in l
      end

  end

  fun layout opts (p, config) =
      let
        val s = stateMk ()
        val e = envMk (config, opts)
        val () = MA.analyseProgram (s, e, p)
        val l = layoutStats (opts, s)
      in l
      end

  fun program (config, opts, p, out) =
      Layout.outputWidth (layout opts (p, config), 78, out)

end;
