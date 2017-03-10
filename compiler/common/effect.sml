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


(* Effects *)

signature EFFECT =
sig

  eqtype set 
  eqtype effect

  val Total     : set
  val PAny      : set  (* Any P level effect *)
  val ReadOnly  : set  (* New or Read *)
  val Heap      : set  (* Any heap ops *)
  val Init      : set  (* InitGen/InitRead/InitWrite *)
  val HeapInit  : set  (* Heap or Init *)
  val Control   : set  (* Any control, including Partial*) 
  val Any       : set  (* All effects *)

  val Partial   : effect 
  val Io        : effect 
  val HeapGen   : effect 
  val HeapRead  : effect 
  val HeapWrite : effect 
  val Throws    : effect 
  val Returns   : effect 
  val Fails     : effect

  val InitGen   : effect  (* allocates not fully initialised objects *)
  val InitRead  : effect  (* reads from fields not initialised at allocation *)
  val InitWrite : effect  (* initialises fields not at allocation *)

  val PartialS   : set
  val IoS        : set
  val HeapGenS   : set
  val HeapReadS  : set
  val HeapWriteS : set
  val ThrowsS    : set
  val ReturnsS   : set
  val FailsS     : set

  val InitGenS   : set
  val InitReadS  : set
  val InitWriteS : set
                
  val effects   : effect list

  val isEmpty  : set -> bool                
  val contains : set * effect -> bool
  val subset   : set * set -> bool

  val fromList     : effect list -> set
  val add          : set * effect -> set
  val remove       : set * effect -> set
  val single       : effect -> set
  val union        : set * set -> set
  val unionL       : set list -> set
  val intersection : set * set -> set
  val difference   : set * set -> set

  val effectToChar : effect -> char
  val charToSet    : char -> set option
  val layout       : set -> Layout.t

  val compare      : set * set -> order

end;

structure Effect :> EFFECT = struct

  structure WordN = Word32

  type set = WordN.word
  type effect = WordN.word
                 
  val Total     : set = WordN.fromInt 0x0

  fun lshift (a,b) = WordN.<<(WordN.fromInt a, WordN.fromInt b)

  val Partial   : effect = lshift(1, 0)
  val Io        : effect = lshift(1, 1)
  val HeapGen   : effect = lshift(1, 2)
  val HeapRead  : effect = lshift(1, 3)
  val HeapWrite : effect = lshift(1, 4)
  val Throws    : effect = lshift(1, 5)
  val Returns   : effect = lshift(1, 6)
  val Fails     : effect = lshift(1, 7)

  val InitGen   : effect = lshift(1, 8)
  val InitRead  : effect = lshift(1, 9)
  val InitWrite : effect = lshift(1, 10)

  val PartialS   : set = Partial
  val IoS        : set = Io
  val HeapGenS   : set = HeapGen
  val HeapReadS  : set = HeapRead
  val HeapWriteS : set = HeapWrite
  val ThrowsS    : set = Throws
  val ReturnsS   : set = Returns
  val FailsS     : set = Fails

  val InitGenS   : set = InitGen
  val InitReadS  : set = InitRead
  val InitWriteS : set = InitWrite

  val effects = [Partial, Io, HeapGen, HeapRead, HeapWrite, Throws, Returns, Fails, InitGen, InitRead, InitWrite]

  fun isEmpty (a : set) = (a = (WordN.fromInt 0))                 
  fun contains (a : set, b : effect) = not (WordN.andb(a, b) = (WordN.fromInt 0))
  fun subset (a : set, b : set) = WordN.orb (a, b) = b

  fun add (a : set, b : effect) = WordN.orb(a,b)
  fun remove (a : set, b : effect) = WordN.andb (a, WordN.notb b)
  fun single (a : effect) = a
  fun union (a : set, b : set) = WordN.orb (a, b)
  fun unionL (ss : set list) : set = List.fold (ss, Total, union)
  fun intersection (a : set, b : set) = WordN.andb (a, b)
  fun difference (a : set, b : set) = WordN.andb (a, WordN.notb b)

  fun fromList effects = List.fold (effects, Total, add)

  val PAny     : set = fromList  [Partial, Io, HeapGen, HeapRead, HeapWrite, Throws, Returns, Fails]
  val ReadOnly : set = fromList [HeapGen, HeapRead, InitGen, InitRead]
  val Heap     : set = fromList [HeapGen, HeapRead, HeapWrite]
  val Init     : set = fromList [InitGen, InitRead, InitWrite]
  val HeapInit : set = union (Heap, Init)
  val Control  : set = fromList [Partial, Throws, Returns, Fails]
  val Any      : set = fromList effects

  fun effectToChar f =
      if f = Partial then #"P"
      else if f = Io then #"I"
      else if f =  HeapGen then #"N"
      else if f =  HeapRead then #"R"
      else if f =  HeapWrite then #"W"
      else if f =  Throws then #"T"
      else if f =  Returns then #"X"
      else if f =  Fails then #"F"
      else if f =  InitGen then #"g"
      else if f =  InitRead then #"r"
      else if f =  InitWrite then #"w"
      else Fail.fail ("Effect", "effectToChar", "Impossible effect")

  structure CD = DictF(struct type t = char val compare = Char.compare end)

  val charToSetMap : set CD.t =
      CD.fromList (List.map (effects, fn e => (effectToChar e, single e)) @ [(#"A", PAny), (#"a", Any)])

  fun charToSet c = CD.lookup (charToSetMap, c)

  local
    structure L = Layout
    structure LU = LayoutUtils
  in
  fun layout s = 
      let
        val (l1, s) = if subset (PAny, s) then (L.str "A", difference (s, PAny)) else (L.empty, s)
        fun doOne e = if contains (s, e) then LU.char (effectToChar e) else L.empty
        val ls = List.map (effects, doOne)
        val l = LU.brace (L.seq (l1::ls))
      in l
      end
  end

  val compare = WordN.compare

end;
