--  The Main Program
with MDart;                 use MDart;
with StdTypes;              use StdTypes;
with Math3D;                use Math3D;
with Objects3D;             use Objects3D;
with Spheres3D;             use Spheres3D;
with Cubes3D;               use Cubes3D;
with Cylinders3D;           use Cylinders3D;
with ColorRGB;              use ColorRGB;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with X11Windows;            use X11Windows;

--  Main M-Dart Entry Point

procedure M_Dart is
   MyWindow   : X11Windows.X11Window (512, 512);
   Obj1, Obj2 : Object3D_Ptr := null;
   World_Objs : Obj3D_List;
   HP_List    : HitPoint3D_List;
   HP         : HitPoint3D_Ptr;
   Ray        : Ray3D;
   Ro         : Point3D;
   Rd         : Vector3D;
   Light      : Vector3D;
   Shade      : Color_RGB;
   Tr         : Matrix3D;
   Sc         : Matrix3D;
   Sh         : Matrix3D;
   Rt         : Matrix3D;
   C          : Character;
   hit        : LARGE_INTEGER;

   dx, dy : LARGE_FLOAT;
   sx, sy : LARGE_FLOAT;
   px, py : LARGE_FLOAT;
   ex, ey : LARGE_FLOAT;
   dot    : SMALL_FLOAT;

begin
   Opening_Message;

   MyWindow.OpenWindow;
   MyWindow.RedrawWindow;

   Rd    := ConstructVector3D (0.0, 0.0, -1.0);
   Light := ConstructVector3D (0.0, 0.0, 1.0);

   Ray  := ConstructRay3D (Ro, Rd);
   Obj1 := ConstructCylinder3D (Ada.Strings.Unbounded.To_Unbounded_String ("My Cylinder 1"));

   Tr := ConstructTranslateMatrix3D (0.0, 0.0, 0.0);
   Sc := ConstructScaleMatrix3D (1.0, 1.0, 1.0);
   Rt := ConstructRotXMatrix3D (MyPI / 2.5);
   --  Sh := ConstructShearMatrix3D(1.2, 1.0, 0.5, 0.0, 0.0, 0.0);
   Obj1.SetObj3DTrans (Rt);

   sx := -4.0;
   ex := 4.0;
   sy := -4.0;
   ey := 4.0;
   dx := (ex - sx) / 512.0;
   dy := (ey - sy) / 512.0;

   py := ey;
   for lY in 1 .. 512 loop
      px := sx;
      for lX in 1 .. 512 loop
         Ro  := ConstructPoint3D (px, py, 5.0);
         Ray := ConstructRay3D (Ro, Rd);
         Obj1.Intersect (Ray, HP_List);
         hit := GetHitPoint3DListCount (HP_List);
         if hit > 0 and py > 0.0 then
            HP  := GetFirstHitPoint3DFromHitPoint3DList (HP_List);
            dot := SMALL_FLOAT (Light * GetHitPoint3DLocalNV (HP));
            if dot < 0.1 then
               dot := 0.1;
            end if;

            Shade := ConstructColor_RGB (dot, dot, dot);
            MyWindow.DrawPixel (lX, lY, Color_RGB_To_X11 (Shade * RGB_Red));
         end if;
         FreeHitPoint3DList (HP_List);
         px := px + dx;
      end loop;
      py := py - dy;
   end loop;

   Closing_Message;

   MyWindow.RedrawWindow;
   Put_Line ("Press a key and <ENTER> to continue...");
   Get (C);
   MyWindow.CloseWindow;

end M_Dart;
