structure SDL =
struct

  fun orb32 l = foldl Word32.orb 0w0 l

  type SDL_Rect = {1_x:int, 2_y:int, 3_w:int, 4_h:int}

  val SDL_GetError =
      _import "SDL_GetError"
      : () -> char ptr

  val getTicks =
      _import "SDL_GetTicks"
      : () -> word32

  val delay =
      _import "SDL_Delay"
      : word32 -> ()

  exception SDL of string
  fun checkErrorByNull p =
      if p = Pointer.NULL ()
      then raise SDL (Pointer.importString (SDL_GetError ()))
      else p
  fun checkErrorByNegative n =
      if n < 0
      then raise SDL (Pointer.importString (SDL_GetError ()))
      else ()

  type SDL_InitFlags = word32
  val SDL_INIT_VIDEO = 0wx00000020
  val initFlags = orb32
  val SDL_Init =
      _import "SDL_Init"
      : SDL_InitFlags -> int
  val init = checkErrorByNegative o SDL_Init

  val quit =
      _import "SDL_Quit"
      : () -> ()

  type SDL_Window = unit ptr
  type SDL_Renderer = unit ptr
  type SDL_Surface = unit ptr
  type SDL_Texture = unit ptr

  type SDL_WindowFlags = word32
  val windowFlags = orb32
  val SDL_WINDOW_OPENGL = 0wx00000002
  val SDL_WINDOW_SHOWN = 0wx00000004
  val SDL_WINDOW_RESIZABLE = 0wx00000020
  val SDL_WINDOW_ALLOW_HIGHDPI = 0wx00002000
  val SDL_WINDOWPOS_UNDEFINED = 0x1FFF0000
  val SDL_CreateWindow =
      _import "SDL_CreateWindow"
      : (string, int, int, int, int, SDL_WindowFlags) -> SDL_Window
  val createWindow = checkErrorByNull o SDL_CreateWindow

  val destroyWindow =
      _import "SDL_DestroyWindow"
      : SDL_Window -> ()

  val SDL_GetWindowSize =
      _import "SDL_GetWindowSize"
      : (SDL_Window, int ref, int ref) -> ()
  fun getWindowSize window =
      let
        val w = ref 0
        val h = ref 0
      in
        SDL_GetWindowSize (window, w, h);
        (!w, !h)
      end

  type SDL_GLattr = int
  val SDL_GL_DOUBLEBUFFER = 5
  val SDL_GL_SetAttribute =
      _import "SDL_GL_SetAttribute"
      : (SDL_GLattr, int) -> int
  val GL_setAttribute = checkErrorByNegative o SDL_GL_SetAttribute

  type SDL_GLContext = unit ptr
  val SDL_GL_CreateContext =
      _import "SDL_GL_CreateContext"
      : SDL_Window -> SDL_GLContext
  val GL_createContext = checkErrorByNull o SDL_GL_CreateContext

  val SDL_GL_SetSwapInterval =
      _import "SDL_GL_SetSwapInterval"
      : int -> int
  val GL_setSwapInterval = checkErrorByNegative o SDL_GL_SetSwapInterval

  val GL_swapWindow =
      _import "SDL_GL_SwapWindow"
      : SDL_Window -> ()

  type SDL_RendererFlags = word32
  val rendererFlags = orb32
  val SDL_RENDERER_ACCELERATED = 0wx00000002
  val SDL_RENDERER_PRESENTVSYNC = 0wx00000004
  val SDL_CreateRenderer =
      _import "SDL_CreateRenderer"
      : (SDL_Window, int, SDL_RendererFlags) -> SDL_Renderer
  val createRenderer = checkErrorByNull o SDL_CreateRenderer

  val destroyRenderer =
      _import "SDL_DestroyRenderer"
      : SDL_Renderer -> ()

  val SDL_GetRendererOutputSize =
      _import "SDL_GetRendererOutputSize"
      : (SDL_Renderer, int ref, int ref) -> int
  fun getRendererOutputSize r =
      let
        val w = ref 0
        val h = ref 0
      in
        checkErrorByNegative (SDL_GetRendererOutputSize (r, w, h));
        (!w, !h)
      end

  val SDL_SetRenderDrawColor =
      _import "SDL_SetRenderDrawColor"
      : (SDL_Renderer, word8, word8, word8, word8) -> int
  val setRenderDrawColor = checkErrorByNegative o SDL_SetRenderDrawColor

  val SDL_RenderClear =
      _import "SDL_RenderClear"
      : SDL_Renderer -> int
  val renderClear = checkErrorByNegative o SDL_RenderClear

  val SDL_RenderCopy =
      _import "SDL_RenderCopy"
      : (SDL_Renderer, SDL_Texture, SDL_Rect, SDL_Rect) -> int
  val renderCopy = checkErrorByNegative o SDL_RenderCopy

  val SDL_RenderPresent =
      _import "SDL_RenderPresent"
      : SDL_Renderer -> int
  val renderPresent = checkErrorByNegative o SDL_RenderPresent

  type pixelFormat = word32
  val SDL_PIXELFORMAT_ARGB8888 = 0wx16362004
  val SDL_PIXELFORMAT_RGBA8888 = 0wx16462004
  val SDL_PIXELFORMAT_ABGR8888 = 0wx16762004
  val SDL_PIXELFORMAT_BGRA8888 = 0wx16862004
  type SDL_TextureAccess = int
  val SDL_TEXTUREACCESS_STATIC = 0
  val SDL_TEXTUREACCESS_STREAMING = 1
  val SDL_CreateTexture =
      _import "SDL_CreateTexture"
      : (SDL_Renderer, pixelFormat, SDL_TextureAccess, int, int) -> SDL_Texture
  val createTexture = checkErrorByNull o SDL_CreateTexture

  val destroyTexture =
      _import "SDL_DestroyTexture"
      : SDL_Texture -> ()

  val SDL_LockTexture =
      _import "SDL_LockTexture"
      : (SDL_Texture, SDL_Rect, unit ptr ref, int ref) -> int
  fun lockTexture (t, r) =
      let
        val p = ref (Pointer.NULL ())
        val pitch = ref 0
      in
        checkErrorByNegative (SDL_LockTexture (t, r, p, pitch));
        !p
      end

  val SDL_UnlockTexture =
      _import "SDL_UnlockTexture"
      : SDL_Texture -> int
  val unlockTexture = checkErrorByNegative o SDL_UnlockTexture

  val 'a#unboxed SDL_UpdateTexture =
      _import "SDL_UpdateTexture"
      : (SDL_Texture, SDL_Rect, 'a array, int) -> int
  fun updateTexture x = checkErrorByNegative (SDL_UpdateTexture x)

  type SDL_RWops = unit ptr
  val SDL_RWFromFile =
      _import "SDL_RWFromFile"
      : (string, string) -> SDL_RWops
  val SDL_LoadBMP_RW =
      _import "SDL_LoadBMP_RW"
      : SDL_RWops * int -> SDL_Surface
  fun loadBMP s = 
      checkErrorByNull
        (SDL_LoadBMP_RW (checkErrorByNull (SDL_RWFromFile (s, "rb")), 1))

  val freeSurface =
      _import "SDL_FreeSurface"
      : SDL_Surface -> ()

  val SDL_CreateTextureFromSurface =
      _import "SDL_CreateTextureFromSurface"
      : (SDL_Renderer, SDL_Surface) -> SDL_Texture
  val createTextureFromSurface = checkErrorByNull o SDL_CreateTextureFromSurface

  val sizeof_SDL_Event = 14 (* in 32-bit words *)
  type SDL_Event = word32 array
  type SDL_EventType = word32
  type SDL_WindowEventID = word32
  val SDL_QUIT = 0wx100
  val SDL_WINDOWEVENT = 0wx200
  val SDL_SYSWMEVENT = 0wx201
  val SDL_KEYDOWN = 0wx300
  val SDL_KEYUP = 0wx301
  val SDL_MOUSEMOTION = 0wx400
  val SDL_MOUSEBUTTONDOWN = 0wx401
  val SDL_MOUSEBUTTONUP = 0wx402
  val SDL_WINDOWEVENT_SIZE_CHANGED = 0wx6
  fun newEvent () = Array.array (sizeof_SDL_Event, 0w0) : SDL_Event
  fun eventType e = Array.sub (e, 0) : SDL_EventType
  fun keysym e = Array.sub (e, 5) (* SDL_KeyboardEvent.keysym.sym *)
  fun mouseX e = Array.sub (e, 5) (* SDL_MouseMotion(Button)Event.x *)
  fun mouseY e = Array.sub (e, 6) (* SDL_MouseMotion(Button)Event.y *)
  fun windowEvent e = Array.sub (e, 3) (* SDL_WindowEvent.event *)
  fun windowData1 e = Array.sub (e, 4) (* SDL_WindowEvent.data1 *)
  fun windowData2 e = Array.sub (e, 5) (* SDL_WindowEvent.data2 *)

  val SDL_PollEvent =
      _import "SDL_PollEvent"
      : SDL_Event -> int
  fun pollEvent () =
      let
        val e = newEvent ()
      in
        if SDL_PollEvent e = 0
        then NONE
        else SOME e
      end

end
