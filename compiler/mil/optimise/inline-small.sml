(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_INLINE_SMALL =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end
                             
structure MilInlineSmall :> MIL_INLINE_SMALL = 
struct

  val passname = "MilInlineSmall"

  val stats = [("InlineSmall", "Small/nonrec functions inlined")]

  structure M = Mil
  structure PD = PassData
  structure Cfg = IMil.Cfg
  structure WS = IMil.WorkSet
  structure Use = IMil.Use
  structure MOU = MilOptUtils

  structure Chat = ChatF(struct type env = PD.t
                         val extract = PD.getConfig
                         val name = passname
                         val indent = 0
                         end)
                   


 
  val inlineSmallLimit = 50

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil inline small pass")

  fun debugShowPre (d, imil, fname)  = 
      if Config.debug andalso debugPass (PD.getConfig d) then
        if (Config.debugLevel (PD.getConfig d, passname)) > 0 then 
          let
            val () = print ("Inlining function:\n")
            val () = IMil.printCfg (imil, Cfg.getCfgByName (imil, fname))
            val () = print "\n"
          in ()
          end
        else 
          let
            val () = print ("Inlining function: ")
            val () = IMil.printVar (imil, fname)
            val () = print "\n"
          in ()
          end
        else ()

  fun debugShowPost (d, imil)  = 
      if Config.debug andalso 
         debugPass (PD.getConfig d) andalso
         (Config.debugLevel (PD.getConfig d, passname)) > 1 then 
        let
          val () = print ("After inlining:\n")
          val mil = IMil.unBuild imil
          val () = MilLayout.printGlobalsOnly (PD.getConfig d, mil)
          val () = print "\n"
        in ()
        end
      else ()

  fun tryInlineSmall (d, imil, worklist, (fname, c)) = 
      Try.try
        (fn () => 
            let
              val () = Try.require (Cfg.getSize (imil, c) < inlineSmallLimit)
              val () = Try.require (not (Cfg.getRecursive (imil, c)))
              val uses = Cfg.getUses (imil, c)
              fun getCandidateCall u = 
                  Try.try
                    (fn () => 
                        let
                          val i = Try.<- (MOU.useToIInstr (imil, u))
                          val t = Try.<- (MOU.iinstrToTransfer (imil, i))
                          fun warn f = 
                              if f = fname then ()
                              else 
                                let 
                                  val () = Chat.warn0 (d, 
                                                       "Fun code used in call "^
                                                       "but not callee!")
                                in Try.fail()
                                end
                                
                          fun doConv conv = 
                              (case conv
                                of M.CCode f => warn f
                                 | M.CDirectClosure (f, c) => warn f
                                 | _ => Try.fail())

                          val () = 
                              case t
                               of M.TCall (conv, _, _, _, _) => doConv conv
                                | M.TTailCall (conv, _, _) => doConv conv
                                | _ => Try.fail()
                        in i
                        end)

              val calls = Vector.keepAllMap (uses, getCandidateCall)
              val () = Try.require (Vector.length calls > 0)
              val () = debugShowPre (d, imil, fname)
              val ils = Vector.map (calls, 
                                 fn call => Cfg.inlineCopy (imil, fname, call))
              val () = PD.click (d, "InlineSmall")
              val () = debugShowPost (d, imil)
              val () = 
                  Vector.foreach 
                    (ils, 
                     (fn is =>           
                         List.foreach (is, fn i => WS.addInstr (worklist, i))))
              val () = WS.addItem (worklist, IMil.ICode c)
            in ()
            end)
      

  fun inlineSmall (d, imil, w) = 
      let
        val cfgs = Cfg.getCfgs imil
        fun help c = 
            let
              val () = 
                  if Try.bool (tryInlineSmall (d, imil, w, c)) then
                    MilSimplify.simplify (d, imil, w)
                  else ()
            in ()
            end
        val () = List.foreach (cfgs, help)
      in ()
      end

  fun program (imil, d) = 
      let
        val w = WS.new()
        val () = inlineSmall (d, imil, w)
        val () = PD.report (d, passname)
      in ()
      end

  val description = {name        = passname,
                     description = "Inline small functions",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = [debugPassD],
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end
