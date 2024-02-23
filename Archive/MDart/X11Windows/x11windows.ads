--  Package X11Windows
with X;
with X.Xlib;
with Interfaces.C;

--  This package provides some crude Windows plotting support, although not very sexy
--  But it suffices, I guess... It uses the X11 Ada bindings as per x11ada_v1.30

package X11Windows is

   --  This defined the maximum Windows resolution that is supported Not used anywhere at this
   --  time...
   MAX_XSIZE : constant Integer := 1280;
   MAX_YSIZE : constant Integer := 1024;

   --  FrameBuffer type must be defined here as a generic array because it needs to be defined prior
   --  to the protected type
   type FrameBuffer is
     array (Interfaces.C.unsigned range <>,
            Interfaces.C.unsigned range <>)
            of Interfaces.C.unsigned_long;

   --  X11Window is a protected type, taking the window XSIZE and YSIZE as parameters. Note that the
   --  PixelMap is therefore indexed from 1..XSize, 1..YSize, while the actual X11 coordinates will
   --  range from 0..XSize-1, 0..YSize-1
   protected type X11Window (XS, YS : Interfaces.C.unsigned) is
      --  OpenWindow opens the Window as per the initialized XSize, YSize;
      procedure OpenWindow;
      --  CloseWindow closes the Window
      procedure CloseWindow;
      --  ProcessEvents processes any pending Events. NULL right now...
      procedure ProcessEvents;
      --  RedrawWindow redraws the content of the PixelMap into the Window
      procedure RedrawWindow;
      --  DrawPixel sets the (Xc,Yc) pixel in PixelMap to Colour and draws that Colour into (Xc-1,
      --  Yc-1) in the Window
      procedure DrawPixel (Xc, Yc : in Integer; Colour : in Integer);
   private
      --  all the local parameters of the protected object mostly typical X11 stuff
      XSize     : Interfaces.C.unsigned := XS;
      YSize     : Interfaces.C.unsigned := YS;
      PixelMap  : FrameBuffer (1 .. XS, 1 .. YS);
      GCon      : X.Xlib.GC;
      Screen    : Interfaces.C.int;
      Display   : aliased X.Xlib.XDisplay_access;
      Win       : aliased X.Window;
      GCon_Vals : aliased X.Xlib.XGCValues;
      Event     : aliased X.Xlib.XEvent;
   end X11Window;

end X11Windows;
