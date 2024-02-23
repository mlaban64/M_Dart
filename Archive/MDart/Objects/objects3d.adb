--  package Objects3D body
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Integer_Text_IO;        use Ada.Integer_Text_IO;
with Ada.Tags;                   use Ada.Tags;
with Ada.Unchecked_Deallocation;

package body Objects3D is

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   --  Put procedure to print an Object3D
   procedure Put (Obj : in Object3D'Class) is
   begin
      Put_Line ("BEGIN Object3D");
      Put ("Class = ");
      Put (Ada.Tags.Expanded_Name (Obj'Tag));
      Put_Line ("");
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Obj.Name));
      Put_Line ("");
      Put_Line ("Trans = ");
      Put (Obj.Trans);
      Put_Line ("");
      Put_Line ("Trans_Inv = ");
      Put (Obj.Trans_Inv);
      Put_Line ("END Object3D");
   end Put;

   --  Limited Put procedure to print an Object3D
   --  without the matrices
   procedure Limited_Put (Obj : in Object3D'Class) is
   begin
      Put_Line ("BEGIN Object3D");
      Put ("  Class = ");
      Put (Expanded_Name (Obj'Tag));
      Put_Line ("");
      Put ("  Name = ");
      Put (To_String (Obj.Name));
      Put_Line ("");
      Put_Line ("END Object3D");
   end Limited_Put;

   --  Print a Limited_Put of all Object3D's in an Obj3D_List;
   procedure List3D_Put (List3D : in Obj3D_List) is
      Elem : Obj3D_List_Element_Ptr;
      Cnt  : LARGE_INTEGER;
   begin
      Put_Line ("BEGIN Obj3D_List");
      Put ("List contains ");
      Put (Integer (List3D.Count));
      Put_Line (" elements");
      Elem := List3D.First;
      Cnt  := 1;
      Put_Line ("");
      while Elem /= null loop
         Put ("ELEMENT ");
         Put (Integer (Cnt));
         Put_Line ("");
         Limited_Put (Elem.all.Obj.all);
         Elem := Elem.all.Next;
         Cnt  := Cnt + 1;
      end loop;
   end List3D_Put;

   --  Print a HitPoint3D
   procedure Put (HitPoint : in HitPoint3D) is
   begin
      Put_Line ("BEGIN HitPoint3D");
      Put ("Lambda = ");
      Put (HitPoint.Lambda);
      Put_Line ("");
      Put_Line ("HitPoint (local) = ");
      Put (HitPoint.LocalHP);
      Put_Line ("Normal (local) = ");
      Put (HitPoint.LocalNV);
      Put_Line ("");
      Put_Line ("HitPoint (world) = ");
      Put (HitPoint.WorldHP);
      Put_Line ("Normal (world) = ");
      Put (HitPoint.WorldNV);
      Put_Line ("END HitPoint3D");
   end Put;

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   --  Construct an Obj3D_List_Element
   function ConstructObj3D_List_Element return Obj3D_List_Element_Ptr is
      newElem : Obj3D_List_Element_Ptr;
   begin
      newElem := new Obj3D_List_Element;
      return newElem;
   end ConstructObj3D_List_Element;

   --  Construct an HitPoint3D_List_Element
   function ConstructHitPoint3D_List_Element return HitPoint3D_List_Element_Ptr is
      newElem : HitPoint3D_List_Element_Ptr;
   begin
      newElem := new HitPoint3D_List_Element;
      return newElem;
   end ConstructHitPoint3D_List_Element;

   --  Construct a HitPoint3D
   function ConstructHitPoint3D return HitPoint3D_Ptr is
      newElem : HitPoint3D_Ptr;
   begin
      newElem := new HitPoint3D;
      return newElem;
   end ConstructHitPoint3D;

   --  Release a HitPoint3D from memory
   procedure Free is new Ada.Unchecked_Deallocation (HitPoint3D, HitPoint3D_Ptr);

   --  Release a HitPoint3D_List_Element from memory
   procedure Free is new Ada.Unchecked_Deallocation (
      HitPoint3D_List_Element,
      HitPoint3D_List_Element_Ptr);

   --  Release a HitPoint3D_List
   procedure FreeHitPoint3DList (HP3DList : in out HitPoint3D_List) is
      firstElem, nextElem : HitPoint3D_List_Element_Ptr;
   begin
      firstElem := HP3DList.First;
      while firstElem /= null loop
         Free (firstElem.all.HP3D_Ptr);
         nextElem := firstElem.all.Next;
         Free (firstElem);
         firstElem := nextElem;
      end loop;
      HP3DList.First := null;
      HP3DList.Count := 0;
   end FreeHitPoint3DList;

   ----------------------------------------------
   --  Object3D LIST MANIPULATION SUBPROGRAMS  --
   ----------------------------------------------

   --  Add Obj3D to List3D
   procedure AddObject3DToObj3DList (Obj3D : in Object3D_Ptr; List3D : in out Obj3D_List) is
      newElem : Obj3D_List_Element_Ptr;
   begin
      newElem          := ConstructObj3D_List_Element;
      newElem.all.Obj  := Obj3D;
      newElem.all.Next := List3D.First;
      List3D.First     := newElem;
      List3D.Count     := List3D.Count + 1;
   end AddObject3DToObj3DList;

   --  Get the counter of a Obj3D_List
   function GetObj3DListCount (List3D : in Obj3D_List) return LARGE_INTEGER is
   begin
      return List3D.Count;
   end GetObj3DListCount;

   ------------------------------------------------
   --  HitPoint3D LIST MANIPULATION SUBPROGRAMS  --
   ------------------------------------------------
   --  Add HitPoint3D to HitPoint3D_List
   procedure AddHitPoint3DToHitPoint3DList
     (HP3D_Ptr : in HitPoint3D_Ptr;
      HP3DList : in out HitPoint3D_List)
   is
      newElem : HitPoint3D_List_Element_Ptr;

   begin
      --  Create the new List Element and assign the HP to it
      newElem              := ConstructHitPoint3D_List_Element;
      newElem.all.HP3D_Ptr := HP3D_Ptr;

      --  If we have 1 element or more, we need to sort it somehow...
      if HP3DList.Count > 0 then
         --  If the HP is closer, put it in the front of the list
         if HP3D_Ptr.all.Lambda < HP3DList.First.all.HP3D_Ptr.all.Lambda then
            newElem.all.Next := HP3DList.First;
            HP3DList.First   := newElem;
         else --  Put it in second place
            newElem.all.Next        := HP3DList.First.all.Next;
            HP3DList.First.all.Next := newElem;
         end if;
      else --  First element, so just add to the front
         newElem.all.Next := HP3DList.First;
         HP3DList.First   := newElem;
      end if;
      --  Increment the counter of the list
      HP3DList.Count := HP3DList.Count + 1;
   end AddHitPoint3DToHitPoint3DList;

   --  Get the counter of a HitPoint3D_List
   function GetHitPoint3DListCount (HP3DList : in HitPoint3D_List) return LARGE_INTEGER is
   begin
      return HP3DList.Count;
   end GetHitPoint3DListCount;

   --  Get the first HitPoint3D from a List
   function GetFirstHitPoint3DFromHitPoint3DList
     (HP3DList : in HitPoint3D_List)
      return     HitPoint3D_Ptr
   is
   begin
      return HP3DList.First.all.HP3D_Ptr;
   end GetFirstHitPoint3DFromHitPoint3DList;
   -----------------------------------------
   --  CLASS-WIDE HITPOINT3D SUBPROGRAMS  --
   -----------------------------------------

   --  SetHitPoint3DLambda sets the Lambda on a HitPoint3D
   procedure SetHitPoint3DLambda (Lambda : in LARGE_FLOAT; HP3D_Ptr : in HitPoint3D_Ptr) is
   begin
      HP3D_Ptr.all.Lambda := Lambda;
   end SetHitPoint3DLambda;

   --  GetHitPoint3DLambda sets the Lambda on a HitPoint3D
   function GetHitPoint3DLambda (HP3D_Ptr : in HitPoint3D_Ptr) return LARGE_FLOAT is
   begin
      return HP3D_Ptr.all.Lambda;
   end GetHitPoint3DLambda;

   --  SetHitPoint3DLocalHP sets the LocalHP value of an HitPoint3D
   procedure SetHitPoint3DLocalHP (HP : in Point3D; HP3D_Ptr : in HitPoint3D_Ptr) is
   begin
      HP3D_Ptr.all.LocalHP := HP;
   end SetHitPoint3DLocalHP;

   --  GetHitPoint3DLocalHP gets the LocalHP value of an HitPoint3D
   function GetHitPoint3DLocalHP (HP3D_Ptr : in HitPoint3D_Ptr) return Point3D is
   begin
      return HP3D_Ptr.all.LocalHP;
   end GetHitPoint3DLocalHP;

   --  SetHitPoint3DLocalNV sets the LocalNV value of an HitPoint3D
   procedure SetHitPoint3DLocalNV (NV : in Normal3D; HP3D_Ptr : in HitPoint3D_Ptr) is
   begin
      HP3D_Ptr.all.LocalNV := NV;
   end SetHitPoint3DLocalNV;

   --  GetHitPoint3DLocalNV gets the LocalNV value of an HitPoint3D
   function GetHitPoint3DLocalNV (HP3D_Ptr : in HitPoint3D_Ptr) return Normal3D is
   begin
      return HP3D_Ptr.all.LocalNV;
   end GetHitPoint3DLocalNV;

   --  SetHitPoint3DObject sets the object of an HitPoint3D
   procedure SetHitPoint3DObject (Obj : in Object3D; HP3D_Ptr : in HitPoint3D_Ptr) is
   begin
      HP3D_Ptr.all.Obj := Obj;
   end SetHitPoint3DObject;

   --  GetHitPoint3DObject gets the object of an HitPoint3D
   function GetHitPoint3DObject (HP3D_Ptr : in HitPoint3D_Ptr) return Object3D is
   begin
      return HP3D_Ptr.all.Obj;
   end GetHitPoint3DObject;

   ---------------------------------------
   --  CLASS-WIDE OBJECT3D SUBPROGRAMS  --
   ---------------------------------------

   --  SetObj3DName sets the Name of an Object3D
   procedure SetObj3DName (Obj3D : in out Object3D'Class; Name : in Unbounded_String) is
   begin
      Obj3D.Name := Name;
   end SetObj3DName;

   --  GetObj3DName returns the Name of an Object3D
   function GetObj3DName (Obj3D : in Object3D'Class) return Unbounded_String is
   begin
      return Obj3D.Name;
   end GetObj3DName;

   --  SetObj3DTrans sets the Trans Matrix of an Object3D
   procedure SetObj3DTrans (Obj3D : in out Object3D'Class; Mat : in Matrix3D) is
   begin
      Obj3D.Trans     := Mat;
      Obj3D.Trans_Inv := Inverse (Mat);
   end SetObj3DTrans;

   --  GetObj3DTrans returns the Trans Matrix of an Object3D
   function GetObj3DTrans (Obj3D : in Object3D'Class) return Matrix3D is
   begin
      return Obj3D.Trans;
   end GetObj3DTrans;

   --  GetObj3DTrans_Inv returns the Trans Matrix of an Object3D
   function GetObj3DTrans_Inv (Obj3D : in Object3D'Class) return Matrix3D is
   begin
      return Obj3D.Trans_Inv;
   end GetObj3DTrans_Inv;

   --  Intersect an Object with a Ray
   procedure Intersect (Obj3D : in Object3D; Ray : in Ray3D; HP_List : in out HitPoint3D_List) is
   begin
      null;
   end Intersect;

end Objects3D;
