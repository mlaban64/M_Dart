--  Package X11Windows body

package body X11Windows is

   protected body X11Window is

      --  OpenWindow will open the window that is defined by the protected variable parameters,
      --  clear the PixelMap and return
      procedure OpenWindow is
         use X;
         use X.Xlib;
         use Interfaces.C;
      begin

         Display := XOpenDisplay (null);
         --  Catch an error, if any
         if Display = null then
            raise Program_Error;
         end if;

         --  Do all the necessary X11 stuff...copied from CURVES.ADB in the Ada x11 1.30 library
         Screen                   := DefaultScreen (Display);
         Win                      :=
            XCreateSimpleWindow
              (Display,
               RootWindow (Display, Screen),
               0,
               0,
               XSize,
               YSize,
               2,
               WhitePixel (Display, Screen),
               BlackPixel (Display, Screen));
         GCon_Vals.foreground     := WhitePixel (Display, Screen);
         GCon_Vals.background     := BlackPixel (Display, Screen);
         GCon_Vals.subwindow_mode := CoordModeOrigin;
         GCon                     :=
            XCreateGC
              (Display,
               Drawable (Win),
               GCForeground + GCBackground + GCSubwindowMode,
               GCon_Vals'Access);
         XMapRaised (Display, Win);
         XFlush (Display);
         XClearWindow (Display, Win);

         --  Clear the PixelMap
         for Xc in 1 .. XSize loop
            for Yc in 1 .. YSize loop
               PixelMap (Xc, Yc) := 0;
            end loop;
         end loop;

      end OpenWindow;

      --  Close the window
      procedure CloseWindow is
         use X;
         use X.Xlib;
         use Interfaces.C;
      begin
         XCloseDisplay (Display);
      end CloseWindow;

      --  a static Events Handler, which can be called from anywhere it should peek if an Event
      --  exists, and if so, handle it it should return immediately if no events are queued
      procedure ProcessEvents is
         use X;
         use X.Xlib;
         use Interfaces.C;
      begin
         null;
      end ProcessEvents;

      procedure DrawPixel (Xc, Yc : in Integer; Colour : in Integer) is
         use X;
         use X.Xlib;
         use Interfaces.C;
         XCol : Interfaces.C.unsigned_long;
      begin
         XCol := Interfaces.C.unsigned_long (Colour);
         XSetForeground (Display, GCon, XCol);
         XDrawPoint (Display, Drawable (Win), GCon, X.signed_int (Xc - 1), X.signed_int (Yc - 1));
         PixelMap (Interfaces.C.unsigned (Xc), Interfaces.C.unsigned (Yc))   := XCol;
      end DrawPixel;

      procedure RedrawWindow is
         use X;
         use X.Xlib;
         use Interfaces.C;
         XCol : Interfaces.C.unsigned_long;
      begin
         for Xc in 1 .. XSize loop
            for Yc in 1 .. YSize loop
               XCol := PixelMap (Xc, Yc);
               XSetForeground (Display, GCon, XCol);
               XDrawPoint
                 (Display,
                  Drawable (Win),
                  GCon,
                  X.signed_int (Xc - 1),
                  X.signed_int (Yc - 1));
            end loop;
         end loop;
      end RedrawWindow;

   end X11Window;

end X11Windows;
