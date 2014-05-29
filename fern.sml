
(**
 * `shida` image generation by StandardML
 * shida means a fern in Japanese.
 *
 * inspired by
 *   id:nakamura001 2014/05/05 shida generation by Processing
 *   http://d.hatena.ne.jp/nakamura001/20140505/1399316565
 *
 * howto
 *   $ sml
 *   - CM.make "sources.cm";
 *   - Fern.main (CommandLine.name(), CommandLine.arguments());
 *   - ^D
 *   $ pnmtopng fern.ppm > fern.png
 *)
structure Fern :
sig
  val main : (string * string list) -> int
end
=
struct
  val (WIDTH, HEIGHT) = (500, 500)
  val N = 20

  type word = Word8.word

  (* rgb *)
  type color = word * word * word

  val bgcolor   : color = (0w255, 0w255, 0w255)
  val linecolor : color = (0w0  , 0w128, 0w0  )

  fun create_surface {height,width} =
    Word8Array.array (height*width*3, 0w0)

  fun W1x x y =  0.836*x + 0.044*y
  fun W1y x y = ~0.044*x + 0.836*y + 0.169
  fun W2x x y = ~0.141*x + 0.302*y
  fun W2y x y =  0.302*x + 0.141*y + 0.127
  fun W3x x y =  0.141*x - 0.302*y
  fun W3y x y =  0.302*x + 0.141*y + 0.169
  fun W4x x y =  0.0
  fun W4y x y =  0.175337 * y

  local
    val rand_state = Random.rand (48271, valOf Int.maxInt)
  in
    fun rand () = Random.randReal rand_state
  end

  fun set_pixel surface x y (color : color) =
    (Word8Array.update(surface, 3*(y*WIDTH+x) +0, #1 color);
     Word8Array.update(surface, 3*(y*WIDTH+x) +1, #2 color);
     Word8Array.update(surface, 3*(y*WIDTH+x) +2, #3 color)
    )

  fun fill_surface surface (bg:color) =
    let
      val range_w = List.tabulate(WIDTH ,fn i=>i)
      val range_h = List.tabulate(HEIGHT,fn i=>i)
      fun line y =
        app (fn x=> set_pixel surface x y bg) range_w
    in
      app line range_h
    end

  fun render surface k x y =
    if 0 < k
    then
      let
        fun render' (fx, fy) = render surface (k-1) (fx x y) (fy x y)
      in
        app (fn (true ,fx,fy)=> render' (fx,fy)
              | (false, _, _)=> ()
            )
            [(true        , W1x,W1y)
            ,(rand() < 0.3, W2x,W2y)
            ,(rand() < 0.3, W3x,W3y)
            ,(rand() < 0.3, W4x,W4y)
            ]
      end
    else
      let
        val s = 490.0
        val x = Real.floor (x*s+(real WIDTH)*0.5)
        val y = Real.floor (real HEIGHT - y*s)
      in
        set_pixel surface x y linecolor
      end

  fun saveppm fname w h img =
    let
      val file = BinIO.openOut fname
      fun write_str fp ss =
        Substring.app (fn c=> BinIO.output1 (fp, Word8.fromInt (ord c))) (Substring.full ss)
    in
      write_str file "P6\n";
      write_str file (Int.toString w^" "^Int.toString h^"\n");
      write_str file "255\n";
      BinIO.output (file, Word8Array.vector img);
      BinIO.closeOut file
    end

  fun main (name,args) =
    let
      val surface = create_surface {height=HEIGHT, width=WIDTH}
      val () = fill_surface surface bgcolor
      val () = print "rendering fern...\n";
    in
      render surface N 0.0 0.0;
      saveppm 
        (case args of file::_ => file | _=> "fern.ppm")
        WIDTH HEIGHT surface;
      OS.Process.success
    end
    handle exn => (print(exnMessage exn); OS.Process.failure)
end

