with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Tags;               use Ada.Tags;
with Ada.Unchecked_Deallocation;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body ShadePoints is
   -----------------
   -- ADT ShadePoint
   -----------------

   function Construct_ShadePoint (Hp : HitPoint; Obj_Ptr : in Object_Ptr) return ShadePoint_Ptr is
      Sp_Ptr : ShadePoint_Ptr;
   begin
      Sp_Ptr         := new ShadePoint;
      Sp_Ptr.Hp      := Hp;
      Sp_Ptr.Obj_Ptr := Obj_Ptr;
      return Sp_Ptr;
   end Construct_ShadePoint;

   procedure Put (Sp_Ptr : in ShadePoint_Ptr; Msg : in String := "ShadePoint ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put (Sp_Ptr.Hp, "HitPoint = ");
      Put (Sp_Ptr.Obj_Ptr.all);
      Put_Line ("END " & Msg);
   end Put;

   function Get_Object (Sp_Ptr : in ShadePoint_Ptr) return Object_Ptr is (Sp_Ptr.Obj_Ptr);

   procedure Compute_Hitpoint_World (Sp_Ptr : in ShadePoint_Ptr; Extra_Flip : in Boolean := False) is
   begin
      if Sp_Ptr.Obj_Ptr.Get_Flip_Normal /= Extra_Flip then
         Flip_Normals (Sp_Ptr.Hp);
      end if;
      Set_WorldHp (Sp_Ptr.Hp, Point_On_Ray (Get_Ray (Sp_Ptr.Hp), Get_Lambda (Sp_Ptr.Hp)));
      Set_WorldNv (Sp_Ptr.Hp, Sp_Ptr.Obj_Ptr.Get_Object_Trans_Inv * Get_ObjectNv (Sp_Ptr.Hp));
   end Compute_Hitpoint_World;

   function Get_HitPoint (Sp_Ptr : in ShadePoint_Ptr) return HitPoint is (Sp_Ptr.Hp);

   procedure Add_ShadePoint (Sp_Ptr : in ShadePoint_Ptr; Sp_List : in out ShadePoint_List) is
      New_Sp_Elem_Ptr, Sp_Elem_Itr : ShadePoint_List_Element_Ptr;
      My_Lambda                    : Large_Float;
   begin

      New_Sp_Elem_Ptr := new ShadePoint_List_Element;
      My_Lambda       := Get_Lambda (Sp_Ptr.Hp);
      --  Assign the passed Sp to the new List Element
      New_Sp_Elem_Ptr.Sp_Ptr := Sp_Ptr;

      --  First shadepoint, so simply add it
      if Sp_List.No_Of_ShadePoints = 0 then
         Sp_List.First_Ptr := New_Sp_Elem_Ptr;
         Sp_List.Last_Ptr  := New_Sp_Elem_Ptr;
      end if;

      --  Second shadepoint. It is before or after the first
      if Sp_List.No_Of_ShadePoints = 1 then
         --  If the Sp's Hp is closer than the first in the list, put it in the front
         if My_Lambda < Get_Lambda (Sp_List.First_Ptr.Sp_Ptr.Hp) then
            New_Sp_Elem_Ptr.Next_Ptr   := Sp_List.First_Ptr;
            Sp_List.First_Ptr.Prev_Ptr := New_Sp_Elem_Ptr;
            Sp_List.First_Ptr          := New_Sp_Elem_Ptr;
         --  Else put it at the end
         else
            New_Sp_Elem_Ptr.Prev_Ptr  := Sp_List.Last_Ptr;
            Sp_List.Last_Ptr.Next_Ptr := New_Sp_Elem_Ptr;
            Sp_List.Last_Ptr          := New_Sp_Elem_Ptr;
         end if;
      end if;

      --  We have more elements, so we need to sort it in the right place
      if Sp_List.No_Of_ShadePoints > 1 then
         --  Sort with the remainder of the list. It must be somewhere inbetween the first and the last. Loop through the list of
         --  remaining shadepoints until the new shadepoint is closer. Start with the 2nd shadepoint

         --  If the Sp's Hp is closer than the first in the list, put it in the front
         if My_Lambda < Get_Lambda (Sp_List.First_Ptr.Sp_Ptr.Hp) then
            New_Sp_Elem_Ptr.Next_Ptr   := Sp_List.First_Ptr;
            Sp_List.First_Ptr.Prev_Ptr := New_Sp_Elem_Ptr;
            Sp_List.First_Ptr          := New_Sp_Elem_Ptr;
         --  If the Sp's Hp is farther than the last in the list, put it at the end
         elsif My_Lambda > Get_Lambda (Sp_List.Last_Ptr.Sp_Ptr.Hp) then
            New_Sp_Elem_Ptr.Prev_Ptr  := Sp_List.Last_Ptr;
            Sp_List.Last_Ptr.Next_Ptr := New_Sp_Elem_Ptr;
            Sp_List.Last_Ptr          := New_Sp_Elem_Ptr;
         else
            Sp_Elem_Itr := Sp_List.First_Ptr.Next_Ptr;
            --  loop until the before-last one
            while My_Lambda > Get_Lambda (Sp_Elem_Itr.Sp_Ptr.Hp) and Sp_Elem_Itr.Next_Ptr /= null loop
               Sp_Elem_Itr := Sp_Elem_Itr.Next_Ptr;
            end loop;
            --  Add it in front of the current element
            New_Sp_Elem_Ptr.Next_Ptr      := Sp_Elem_Itr;
            New_Sp_Elem_Ptr.Prev_Ptr      := Sp_Elem_Itr.Prev_Ptr;
            Sp_Elem_Itr.Prev_Ptr.Next_Ptr := New_Sp_Elem_Ptr;
            Sp_Elem_Itr.Prev_Ptr          := New_Sp_Elem_Ptr;
         end if;
      end if;

      --  Increment the number of shadepoint in this list
      Sp_List.No_Of_ShadePoints := Sp_List.No_Of_ShadePoints + 1;
      if Sp_List.No_Of_ShadePoints > Max_Number_Of_Shadepoints then
         Max_Number_Of_Shadepoints := Sp_List.No_Of_ShadePoints;
      end if;

   end Add_ShadePoint;

   function Get_No_Of_ShadePoints (Sp_List : in ShadePoint_List) return Large_Integer is (Sp_List.No_Of_ShadePoints);

   function Get_First_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr is
   begin
      Sp_List.Current_Ptr := Sp_List.First_Ptr;
      return Sp_List.First_Ptr.Sp_Ptr;
   end Get_First_ShadePoint;

   function Get_Next_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr is
   begin
      --  Set the current element to the next
      Sp_List.Current_Ptr := Sp_List.Current_Ptr.Next_Ptr;
      if Sp_List.Current_Ptr /= null then
         return Sp_List.Current_Ptr.Sp_Ptr;
      else
         return null;
      end if;
   end Get_Next_ShadePoint;

   function Get_Last_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr is
   begin
      Sp_List.Current_Ptr := Sp_List.Last_Ptr;
      return Sp_List.Last_Ptr.Sp_Ptr;
   end Get_Last_ShadePoint;

   function Get_Previous_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr is
   begin
      --  Set the current element to the next
      Sp_List.Current_Ptr := Sp_List.Current_Ptr.Prev_Ptr;
      if Sp_List.Current_Ptr /= null then
         return Sp_List.Current_Ptr.Sp_Ptr;
      else
         return null;
      end if;
   end Get_Previous_ShadePoint;

   function Intersect_Objects_With_Ray (R : in Ray; Obj_List, CSG_Obj_List : in out Object_List) return ShadePoint_List is
      Sp_List : ShadePoint_List;
      Sp_Ptr  : ShadePoint_Ptr;
      Obj_Ptr : Object_Ptr;
      No_Hp   : Natural;
      Hp      : HitPoint;
   begin
      --  First, intersect all non-CSG objects
      Obj_Ptr := Get_First_Object (Obj_List);
      --  For all objects in the list
      while Obj_Ptr /= null loop
         --  Intersect the object
         No_Hp := Obj_Ptr.Intersect (R);
         --  Add the found HP's to the ShadePoint List
         for i in 1 .. No_Hp loop
            Sp_Ptr := Construct_ShadePoint (Pop_HitPoint, Obj_Ptr);
            Add_ShadePoint (Sp_Ptr, Sp_List);
         end loop;
         --  Set the current element to the next
         Obj_Ptr := Get_Next_Object (Obj_List);
      end loop;

      --  Next, intersect all primitive objects that are part of a CSG object
      Obj_Ptr := Get_First_Object (CSG_Obj_List);
      --  For all objects in the list
      while Obj_Ptr /= null loop
         --  Intersect the object
         No_Hp := Obj_Ptr.Intersect (R);
         --  Add the found HP's to the ShadePoint List
         for i in 1 .. No_Hp loop
            Sp_Ptr := Construct_ShadePoint (Pop_HitPoint, Obj_Ptr);
            Add_ShadePoint (Sp_Ptr, Sp_List);
         end loop;
         --  Set the current element to the next
         Obj_Ptr := Get_Next_Object (CSG_Obj_List);
      end loop;

      return Sp_List;
   end Intersect_Objects_With_Ray;

   function Hit_Objects_With_Ray (R : in Ray; Obj_List, CSG_Obj_List : in out Object_List) return ShadePoint_List is
      Sp_List : ShadePoint_List;
      Sp_Ptr  : ShadePoint_Ptr;
      Obj_Ptr : Object_Ptr;
      No_Hp   : Natural;
      Hp      : HitPoint;
   begin
      --  First, hit all non-CSG objects
      Obj_Ptr := Get_First_Object (Obj_List);
      --  For all objects in the list
      while Obj_Ptr /= null loop
         --  Intersect the object
         No_Hp := Obj_Ptr.Hit (R);
         --  Add the found HP's to the ShadePoint List
         for i in 1 .. No_Hp loop
            Sp_Ptr := Construct_ShadePoint (Pop_HitPoint, Obj_Ptr);
            Add_ShadePoint (Sp_Ptr, Sp_List);
         end loop;
         --  Set the current element to the next
         Obj_Ptr := Get_Next_Object (Obj_List);
      end loop;

      --  Next, hit all primitive objects that are part of a CSG object
      Obj_Ptr := Get_First_Object (CSG_Obj_List);
      --  For all objects in the list
      while Obj_Ptr /= null loop
         --  Intersect the object
         No_Hp := Obj_Ptr.Hit (R);
         --  Add the found HP's to the ShadePoint List
         for i in 1 .. No_Hp loop
            Sp_Ptr := Construct_ShadePoint (Pop_HitPoint, Obj_Ptr);
            Add_ShadePoint (Sp_Ptr, Sp_List);
         end loop;
         --  Set the current element to the next
         Obj_Ptr := Get_Next_Object (CSG_Obj_List);
      end loop;

      return Sp_List;
   end Hit_Objects_With_Ray;

   --  Release a ShadePoint from memory
   procedure Free is new Ada.Unchecked_Deallocation (ShadePoint, ShadePoint_Ptr);

   --  Release a ShadePoint_List_Element from memory
   procedure Free is new Ada.Unchecked_Deallocation (ShadePoint_List_Element, ShadePoint_List_Element_Ptr);

   procedure Free_ShadePoint_List (Sp_List : in out ShadePoint_List) is
      First_Elem, Next_Elem : ShadePoint_List_Element_Ptr;
   begin
      First_Elem := Sp_List.First_Ptr;
      while First_Elem /= null loop
         Free (First_Elem.Sp_Ptr);
         Next_Elem := First_Elem.Next_Ptr;
         Free (First_Elem);
         First_Elem := Next_Elem;
      end loop;
      Sp_List.First_Ptr         := null;
      Sp_List.Last_Ptr          := null;
      Sp_List.No_Of_ShadePoints := 0;
   end Free_ShadePoint_List;

   procedure Check_Sort (Sp_List : in ShadePoint_List) is
      Sp_Elem_Ptr : ShadePoint_List_Element_Ptr;
   begin
      Put_Line ("===== BEGIN Lambdas for this ray: =====");
      Sp_Elem_Ptr := Sp_List.First_Ptr;
      while Sp_Elem_Ptr /= null loop
         Put ("LAMBDA = ");
         Put (Get_Lambda (Sp_Elem_Ptr.Sp_Ptr.Hp));
         New_Line;
         Sp_Elem_Ptr := Sp_Elem_Ptr.Next_Ptr;
      end loop;
      Put_Line ("===== END Lambdas for this ray: =====");
   end Check_Sort;

end ShadePoints;
