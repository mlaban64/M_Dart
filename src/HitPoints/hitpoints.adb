with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body HitPoints is
   ---------------
   -- ADT HitPoint
   ---------------

   procedure Push_HitPoint (Lambda : in Large_Float; R : in Ray; ObjectHp : in Point_3D; ObjectNv : in Normal_3D) is
   begin
      --  Check for a stack overflow
      if Hp_Stack.StackPointer >= MAX_HP_STACK then
         Put_Line ("*** ERROR: STACK OVERFLOW in HitPoints.Push_HitPoint");
      else
         Hp_Stack.StackPointer                            := Hp_Stack.StackPointer + 1;
         Hp_Stack.Stack (Hp_Stack.StackPointer).Object_Hp := ObjectHp;
         Hp_Stack.Stack (Hp_Stack.StackPointer).Object_Nv := ObjectNv;
         Hp_Stack.Stack (Hp_Stack.StackPointer).R         := R;
         Hp_Stack.Stack (Hp_Stack.StackPointer).Lambda    := Lambda;
      end if;
   end Push_HitPoint;

   function Pop_HitPoint return HitPoint is
   begin
      --  Check for a stack underflow
      if Hp_Stack.StackPointer <= 0 then
         Put_Line ("*** ERROR: STACK UNDERFLOW in HitPoints.Pop_HitPoint");
         return Hp_Stack.Stack (Hp_Stack.StackPointer);
      else
         Hp_Stack.StackPointer := Hp_Stack.StackPointer - 1;
         return Hp_Stack.Stack (Hp_Stack.StackPointer + 1);
      end if;
   end Pop_HitPoint;

   procedure Put (Hp : in HitPoint; Msg : in String := "HitPoint ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put ("Lambda = ");
      Put (Hp.Lambda);
      New_Line;
      Put (Hp.Object_Hp, "HitPoint (object) = ");
      Put (Hp.Object_Nv, "Normal (object)   = ");
      Put (Hp.World_Hp, "HitPoint (world)  = ");
      Put (Hp.World_Nv, "Normal (world)    = ");
      Put_Line ("END " & Msg);
   end Put;

   function Get_Lambda (Hp : in HitPoint) return Large_Float is (Hp.Lambda);

   function Get_Ray (Hp : in HitPoint) return Ray is (Hp.R);

   function Get_ObjectHp (Hp : in HitPoint) return Point_3D is (Hp.World_Hp);

   function Get_ObjectNv (Hp : in HitPoint) return Normal_3D is (Hp.Object_Nv);

   function Get_WorldHp (Hp : in HitPoint) return Point_3D is (Hp.World_Hp);

   function Get_WorldNv (Hp : in HitPoint) return Normal_3D is (Hp.World_Nv);

   procedure Set_WorldHp (Hp : in out HitPoint; WorldHp : in Point_3D) is
   begin
      Hp.World_Hp := WorldHp;
   end Set_WorldHp;

   procedure Set_WorldNv (Hp : in out HitPoint; WorldNv : in Normal_3D) is
   begin
      Hp.World_Nv := WorldNv;
   end Set_WorldNv;

   procedure Flip_Normals (Hp : in out HitPoint) is
   begin
      Hp.Object_Nv := -Hp.Object_Nv;
   end Flip_Normals;

end HitPoints;
