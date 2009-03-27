(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* Effects for Core P language *)

signature EFFECT = sig
  eqtype set 
  eqtype effect

  val Total     : set
  val PAny      : set  (* Any P level effect *)
  val ReadOnly  : set  (* New or Read *)
  val Heap      : set  (* Any heap ops *)
  val Control   : set  (* Any control, including Partial*) 

  val Partial   : effect 
  val Io        : effect 
  val HeapNew   : effect 
  val HeapRead  : effect 
  val HeapWrite : effect 
  val Throws    : effect 
  val Returns   : effect 
  val Fails     : effect

  (* Non P-level effects *)
  val InitNew   : effect  (* allocates not fully initialised objects *)
  val InitRead  : effect  (* reads from fields not initialised at allocation *)
  val InitWrite : effect  (* initialises fields not at allocation *)

  val PartialS   : set
  val IoS        : set
  val HeapNewS   : set
  val HeapReadS  : set
  val HeapWriteS : set
  val ThrowsS    : set
  val ReturnsS   : set
  val FailsS     : set

  val InitNewS   : set
  val InitReadS  : set
  val InitWriteS : set
                
                
  val effects   : effect list

  val empty    : set -> bool                
  val contains : set * effect -> bool
  val subset   : set * set -> bool

  val fromList     : effect list -> set
  val add          : set * effect -> set
  val remove       : set * effect -> set
  val single       : effect -> set
  val union        : set * set -> set
  val intersection : set * set -> set
  val difference   : set * set -> set
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
  val HeapNew   : effect = lshift(1, 2)
  val HeapRead  : effect = lshift(1, 3)
  val HeapWrite : effect = lshift(1, 4)
  val Throws    : effect = lshift(1, 5)
  val Returns   : effect = lshift(1, 6)
  val Fails     : effect = lshift(1, 7)

  val InitNew   : effect = lshift(1, 8)
  val InitRead  : effect = lshift(1, 9)
  val InitWrite : effect = lshift(1, 10)

  val PartialS   : set = Partial
  val IoS        : set = Io
  val HeapNewS   : set = HeapNew
  val HeapReadS  : set = HeapRead
  val HeapWriteS : set = HeapWrite
  val ThrowsS    : set = Throws
  val ReturnsS   : set = Returns
  val FailsS     : set = Fails

  val InitNewS   : set = InitNew
  val InitReadS  : set = InitRead
  val InitWriteS : set = InitWrite

  val effects =
      [Partial, Io, 
       HeapNew, HeapRead, HeapWrite,
       Throws, Returns, Fails,
       InitNew, InitRead, InitWrite
      ]

  (* Keep these one letter so that layout can be compact *)
  fun effectToString f = 
      if f = Partial then "P"
      else if f = Io then "I"
      else if f =  HeapNew then "N"
      else if f =  HeapRead then "R"
      else if f =  HeapWrite then "W"
      else if f =  Throws then "T"
      else if f =  Returns then "R"
      else if f =  Fails then "F"
      else if f =  InitNew then "iN"
      else if f =  InitRead then "iR"
      else if f =  InitWrite then "iW"
      else raise Fail "Impossible effect"

  fun empty (a : set) = (a = (WordN.fromInt 0))                 
  fun contains (a : set, b : effect) = not (WordN.andb(a, b) = (WordN.fromInt 0))
  fun subset (a : set, b : set) = WordN.orb (a, b) = b

  fun add (a : set,b : effect) = WordN.orb(a,b)
  fun remove (a : set, b : effect) = WordN.andb (a, WordN.notb b)
  fun single (a : effect) = a
  fun union (a : set, b : set) = WordN.orb (a, b)
  fun intersection (a : set, b : set) = WordN.andb (a, b)
  fun difference (a : set, b : set) = WordN.andb (a, WordN.notb b)

  fun layout s = 
      let
        fun doOne (f, str) =
            if contains (s, f) then str ^ (effectToString f) else str
        val str = List.fold (effects, "", doOne)
      in Layout.str ("[" ^ str ^ "]")
      end

  fun fromList effects = List.fold(effects, Total, add)

  val PAny     : set = 
      fromList  [Partial, Io, 
                 HeapNew, HeapRead, HeapWrite,
                 Throws, Returns, Fails
                ]
  val ReadOnly : set = fromList [HeapNew, HeapRead, InitNew, InitRead]
  val Heap     : set = fromList [HeapNew, HeapRead, HeapWrite]
  val Control  : set = fromList [Partial, Throws, Returns, Fails]

  val compare = WordN.compare

end;
