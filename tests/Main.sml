
structure Main = struct
  local
    fun write_src src =
    let
      val path = OS.FileSys.tmpName ()
      val outstream = TextIO.openOut path
      fun body () = TextIO.output (outstream, src)
      fun finally () = TextIO.closeOut outstream
    in
      Try.try body finally;
      print path;
      path
    end

    fun exec src_path =
    let
      val exe_path = "../src/run"
      val proc = Unix.execute (exe_path, [exe_path, src_path])
      val (instream, _) = Unix.streamsOf proc
      val actual = TextIO.inputAll instream
      val _ = Unix.reap proc
    in
      actual
    end

    fun compare_out (SOME (expected)) actual =
        if not (expected = actual) then
          raise Fail actual
        else
          ()
      | compare_out NONE _ = ()

    fun run test =
    let
      val src_path = write_src (#src test)
      val body = fn () => exec src_path
      val finally = fn () => OS.FileSys.remove src_path
      val actual = Try.try body finally
    in
      compare_out (#out test) actual;
      ()
    end

    fun main () =
    let
      val instream = TextIO.openIn (List.hd (CommandLine.arguments ()))
      fun body () = TextIO.inputAll instream
      fun finally () = TextIO.closeIn instream
      val lexbuf = Lexing.createLexerString (Try.try body finally)
      val test = Parser.Test Lexer.Token lexbuf
    in
      run test
    end
  in
    val _ = main ()
  end
end

(*
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
