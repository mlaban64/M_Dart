--  <description>
--  Package Objects3D contains the basic types and functions
--  to manage the 3D Objects that represent the scene to be rendered
--  It also contains the functions to manage hitpoint lists and object hieracrhies
--
--  It is using a generic structure to store a list or tree of objects
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes;              use StdTypes;
with Math3D;                use Math3D;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Objects3D is

   -------------------------------------
   --  TYPE DEFINITIONS FOR OBJECT3D  --
   -------------------------------------

   type Obj3D_List is limited private;
   --  <summary>Obj3D_List  is the root type of all Lists of Objects
   --  It defaults to an empty list, with no elements (Count = 0)</summary>

   type Obj3D_List_Element is limited private;
   --  <summary>Object3D_List_Element  is a linked list element of pointers to Object3D'Class
   --  objects</summary>

   type Object3D is tagged limited private;
   --  <summary>Object3D type is the root type of all objects
   --  It is tagged so it can be used as a Class for dispatching</summary>

   type Object3D_Ptr is access all Object3D'Class;
   --  <summary>Object3D_Ptr is the access type to all Object3D'Class objects</summary>

   type Obj3D_List_Element_Ptr is access all Obj3D_List_Element;
   --  <summary>Obj3D_List_Element_Ptr is the access type to all Obj3D_List_Element'Class
   --  objects</summary>

   ---------------------------------------
   --  TYPE DEFINITIONS FOR HITPOINT3D  --
   ---------------------------------------

   type HitPoint3D_List is limited private;
   --  <summary>HitPoint3D_List  is the root type of all Lists of Objects
   --  It defaults to an empty list, with no elements (Count = 0)</summary>

   type HitPoint3D_List_Element is limited private;
   --  <summary>HitPoint3D_List_Element is a linked list element of pointers to HitPoint3D
   --  objects</summary>

   type HitPoint3D is limited private;
   --  <summary>HitPoint3D type is used to represent a HitPoint in 3D</summary>

   type HitPoint3D_Ptr is access all HitPoint3D;
   --  <summary>HitPoint3D_Ptr is the access type to a HitPoint3D</summary>

   type HitPoint3D_List_Element_Ptr is access all HitPoint3D_List_Element;
   --  <summary>HitPoint3D_List_Element_Ptr is the access type to an
   --  HitPoint_List_Element</summary>

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   procedure Put (Obj : in Object3D'Class);
   --  <summary>Put procedure to print an Object's statistics</summary>
   --  <parameter name="Obj">An object of the Object3D Class Wide Type</parameter>
   --  <exception>No exception</exception>

   procedure Limited_Put (Obj : in Object3D'Class);
   --  <summary>Put procedure to print an Object's statistics, but
   --  without the matrices</summary>
   --  <parameter name="Obj">An object of the Object3D Class Wide Type</parameter>
   --  <exception>No exception</exception>

   procedure List3D_Put (List3D : in Obj3D_List);
   --  <summary>Put procedure to print an Obj3D_List's statistics, but
   --  without the matrices</summary>
   --  <parameter name="Obj">An object of the Object3D Class Wide Type</parameter>
   --  <exception>No exception</exception>

   procedure Put (HitPoint : in HitPoint3D);
   --  <summary>Put procedure to print an HitPoint3D</summary>
   --  <parameter name="HitPoint">A HitPoint3D</parameter>
   --  <exception>No exception</exception>

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructObj3D_List_Element return Obj3D_List_Element_Ptr;
   --  <summary>function to construct an Obj3D_List_Element</summary>
   --  <exception>No exception</exception>

   function ConstructHitPoint3D_List_Element return HitPoint3D_List_Element_Ptr;
   --  <summary>function to construct a HitPoint3D_List_Element</summary>
   --  <exception>No exception</exception>

   function ConstructHitPoint3D return HitPoint3D_Ptr;
   --  <summary>function to construct a HitPoint3D</summary>
   --  <exception>No exception</exception>

   procedure FreeHitPoint3DList (HP3DList : in out HitPoint3D_List);
   --  <summary>FreeHitPoint3DList releases a HitPoint3D_List from memory</summary>
   --  <parameter name="HP3DList">A HitPoint3D_List</parameter>
   --  <exception>No exception</exception>

   ----------------------------------------------
   --  Object3D LIST MANIPULATION SUBPROGRAMS  --
   ----------------------------------------------

   procedure AddObject3DToObj3DList (Obj3D : in Object3D_Ptr; List3D : in out Obj3D_List);
   --  <summary>AddObject3DToObj3DList adds an Object3D to an Obj3D_List at the front</summary>
   --  <parameter name="Obj3D">An object of the Object3D Class Wide Type</parameter>
   --  <parameter name="List3D">An Obj3D_List to add the Obj3D to</parameter>
   --  <exception>No exception</exception>

   function GetObj3DListCount (List3D : in Obj3D_List) return LARGE_INTEGER;
   --  <summary>GetObj3DListCount gets the count of items in an Obj3D_List</summary>
   --  <parameter name="List3D">an Obj3D_List</parameter>
   --  <exception>No exception</exception>

   ------------------------------------------------
   --  HitPoint3D LIST MANIPULATION SUBPROGRAMS  --
   ------------------------------------------------
   procedure AddHitPoint3DToHitPoint3DList
     (HP3D_Ptr : in HitPoint3D_Ptr;
      HP3DList : in out HitPoint3D_List);
   --  <summary>AddHitPoint3DToHitPoint3DList adds an HitPoint3D to an HitPoint3D_List
   --  at the front, if it has a smaller lambda. Else it will put it in second place </summary>
   --  <parameter name="HP3D_Ptr">An object of the HitPoint3D Class Wide Type</parameter>
   --  <parameter name="HP3DList">An HitPoint3D_List to add the HPList3D to</parameter>
   --  <exception>No exception</exception>

   function GetHitPoint3DListCount (HP3DList : in HitPoint3D_List) return LARGE_INTEGER;
   --  <summary>GetHitPoint3DListCount gets the count of items in an HitPoint3D_List</summary>
   --  <parameter name="HP3DList">an HitPoint3D_List</parameter>
   --  <exception>No exception</exception>

   function GetFirstHitPoint3DFromHitPoint3DList
     (HP3DList : in HitPoint3D_List)
      return     HitPoint3D_Ptr;
   --  <summary>GetFirstHitPoint3DFromHitPoint3DList gets the first HitPoint3D_Ptr
   --  from a HitPoint3D_List</summary>
   --  <parameter name="HP3DList">an HitPoint3D_List</parameter>
   --  <exception>No exception</exception>

   -----------------------------------------
   --  CLASS-WIDE HITPOINT3D SUBPROGRAMS  --
   -----------------------------------------
   procedure SetHitPoint3DLambda (Lambda : in LARGE_FLOAT; HP3D_Ptr : in HitPoint3D_Ptr);
   --  <summary>SetHitPoint3DLambda sets the Lambda value of an HitPoint3D</summary>
   --  <parameter name="Lambda">Value of Lambda</parameter>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   function GetHitPoint3DLambda (HP3D_Ptr : in HitPoint3D_Ptr) return LARGE_FLOAT;
   --  <summary>GetHitPoint3DLambda gets the Lambda value of an HitPoint3D</summary>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   procedure SetHitPoint3DLocalHP (HP : in Point3D; HP3D_Ptr : in HitPoint3D_Ptr);
   --  <summary>SetHitPoint3DLocalHP sets the LocalHP value of an HitPoint3D</summary>
   --  <parameter name="HP">The Point3D to be set for LocalHP</parameter>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   function GetHitPoint3DLocalHP (HP3D_Ptr : in HitPoint3D_Ptr) return Point3D;
   --  <summary>GetHitPoint3DLocalHP gets the LocalHP value of an HitPoint3D</summary>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   procedure SetHitPoint3DLocalNV (NV : in Normal3D; HP3D_Ptr : in HitPoint3D_Ptr);
   --  <summary>SetHitPoint3DLocalNV sets the LocalNV value of an HitPoint3D</summary>
   --  <parameter name="HP">The Normal3D to be set for LocalNV</parameter>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   function GetHitPoint3DLocalNV (HP3D_Ptr : in HitPoint3D_Ptr) return Normal3D;
   --  <summary>GetHitPoint3DLocalNV gets the LocalNV value of an HitPoint3D</summary>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   procedure SetHitPoint3DObject (Obj : in Object3D; HP3D_Ptr : in HitPoint3D_Ptr);
   --  <summary>SetHitPoint3DObject sets the object of an HitPoint3D</summary>
   --  <parameter name="ObjPtr">The Object hit</parameter>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   function GetHitPoint3DObject (HP3D_Ptr : in HitPoint3D_Ptr) return Object3D;
   --  <summary>GetHitPoint3DObject gets the object of an HitPoint3D</summary>
   --  <parameter name="HP3D_Ptr">Pointer to a HitPoint3D</parameter>
   --  <exception>No exception</exception>

   ---------------------------------------
   --  CLASS-WIDE OBJECT3D SUBPROGRAMS  --
   ---------------------------------------

   procedure SetObj3DName (Obj3D : in out Object3D'Class; Name : in Unbounded_String);
   --  <summary>SetObj3DName sets the Name of an Object3D</summary>
   --  <parameter name="Obj3D">A class-wide Object3D</parameter>
   --  <parameter name="Name">The new Name of the Object3D</parameter>
   --  <exception>No exception</exception>

   function GetObj3DName (Obj3D : in Object3D'Class) return Unbounded_String;
   --  <summary>GetObj3DName returns the Name of an Object3D</summary>
   --  <parameter name="Obj3D">A class-wide Object3D</parameter>
   --  <exception>No exception</exception>

   function GetObj3DTrans (Obj3D : in Object3D'Class) return Matrix3D;
   --  <summary>GetObj3DTrans returns the Trans Matrix of an Object3D</summary>
   --  <parameter name="Obj3D">A class-wide Object3D</parameter>
   --  <exception>No exception</exception>

   procedure SetObj3DTrans (Obj3D : in out Object3D'Class; Mat : in Matrix3D);
   --  <summary>SetObj3DTrans sets the Trans Matrix of an Object3D
   --  It also recalculates the Trans_Inv as the inverse of Mat</summary>
   --  <parameter name="Obj3D">A class-wide Object3D</parameter>
   --  <parameter name="Mat">A Matrix3D</parameter>
   --  <exception>No exception</exception>

   function GetObj3DTrans_Inv (Obj3D : in Object3D'Class) return Matrix3D;
   --  <summary>GetObj3DTrans_Inv returns the Trans Matrix of an Object3D</summary>
   --  <parameter name="Obj3D">A class-wide Object3D</parameter>
   --  <exception>No exception</exception>

   procedure Intersect (Obj3D : in Object3D; Ray : in Ray3D; HP_List : in out HitPoint3D_List);
   --  <summary>Intersect tries to intersect an Object3D by a Ray3D</summary>
   --  <parameter name="Obj3D">An object of the Object3D Class Wide Type</parameter>
   --  <parameter name="Ray">A Ray that is intersected with Obj3D</parameter>
   --  <parameter name="HP_List">The HitPoint List to which any hitpoints are added</parameter>
   --  <exception>No exception</exception>

private

   type Obj3D_List_Element is record
      Obj : Object3D_Ptr := null;
      --  Pointer to the Object3D
      Next : Obj3D_List_Element_Ptr := null;
      --  Pointer to the next elemant
   end record;

   type Obj3D_List is record
      First : Obj3D_List_Element_Ptr := null;
      --  Pointer to the first element
      Count : LARGE_INTEGER := 0;
      --  Static counter of elements in this list
   end record;

   type Object3D is tagged limited record
      Name : Unbounded_String := To_Unbounded_String ("NOT DEFINED");
      --  The Name of the Object3D
      Trans : Matrix3D;
      --  The transformation applied to this object
      Trans_Inv : Matrix3D;
      --  The inverse of Trans
   end record;

   type HitPoint3D_List is record
      First : HitPoint3D_List_Element_Ptr := null;
      Count : LARGE_INTEGER               := 0;
   end record;

   type HitPoint3D_List_Element is record
      HP3D_Ptr : HitPoint3D_Ptr              := null;
      Next     : HitPoint3D_List_Element_Ptr := null;
   end record;

   type HitPoint3D is record
      LocalHP : Point3D;
      --  LocalHP is the actual hitpoint in local object coordinates;
      WorldHP : Point3D;
      --  WorldHP is the actual hitpoint in world coordinates;
      LocalNV : Normal3D;
      --  Normal at LocalHP;
      WorldNV : Normal3D;
      --  Normal at WorldHP;
      Obj : Object3D;
      --  Pointer to object that is hit;
      Lambda : LARGE_FLOAT := 0.0;
      --  The Lambda parameter of the ray as in HP := Ray.Org + Lambda*Ray.Dir
   end record;

end Objects3D;
