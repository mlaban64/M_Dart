with Core_Types;            use Core_Types;
with Linear_Math;           use Linear_Math;
with Materials;             use Materials;
with HitPoints;             use HitPoints;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The Objects package contains all Abstract Data Types that make up the 3D Objects in a scene</summary>
--<description>The Objects package contains all Abstract Data Types that make up the 3D Objects in a scene.
--The actual objects definitions and methods for a particular object type will be handled in separate child packages</description>
--<group>Objects</group>
package Objects is

   --------------
   --  ADT Object
   --------------

   type Object_List is private;
   --<summary>ADT for the root type of all Lists of Objects. It defaults to an empty list, with no elements (Count = 0)</summary>

   type Object_List_Element is private;
   --<summary>ADT of a linked list element of pointers to Object'Class objects</summary>

   type Object is tagged private;
   --<summary>ADT of the root type of all primary objects. It is tagged so it can be used as a Class for dispatching</summary>

   type Object_Ptr is access all Object'Class;
   --<summary>The access type to all Object'Class objects</summary>

   type Object_List_Element_Ptr is access all Object_List_Element;
   --<summary>The access type to all Object_List_Element'Class objects</summary>

   ------------------------
   -- ADT Object Functions
   ------------------------

   procedure Put (Obj : in Object; Msg : in String := "Object ");
   --<summary>Prints an Object's statistics</summary>
   --<description>Prints an Object's statistics</description>
   --<parameter name="Obj">An object of the Object Class Wide Type</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Obj_List : in out Object_List; Msg : in String := "Object List ");
   --<summary>Prints an Object List</summary>
   --<description>Prints an Object List by traversing the list and printing each object</description>
   --<parameter name="Obj_List">An Object_List</parameter>
   --<exception>None at this moment</exception>

   procedure Add_Object (Obj : in Object_Ptr; Obj_List : in out Object_List);
   --<summary>Adds an Object to an Object_List</summary>
   --<description>Adds an Object to an Object_List</description>
   --<parameter name="Obj">Pointer to the object to be added to the list</parameter>
   --<parameter name="Obj_List">The list to add the object to</parameter>
   --<exception>None at this moment</exception>

   function Get_No_Of_Objects (Obj_List : in Object_List) return Large_Integer;
   --<summary>Returns the number of objects in an Object_List</summary>
   --<description>Function to return the number of objects in an Object_List</description>
   --<parameter name="Obj_List">The list to return the number of objects from</parameter>
   --<exception>None at this moment</exception>

   function Get_First_Object (Obj_List : in out Object_List) return Object_Ptr;
   --<summary>Returns the first object in an Object_List</summary>
   --<description>Function to return the first object in an Object_List</description>
   --<parameter name="Obj_List">The list to return the first object from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Object (Obj_List : in out Object_List) return Object_Ptr;
   --<summary>Returns the next object in an Object_List</summary>
   --<description>Function to return the next object in an Object_List</description>
   --<parameter name="Obj_List">The list to return the next object from</parameter>
   --<exception>None at this moment</exception>

   function Get_Object_Name (Obj : in Object'Class) return Unbounded_String;
   --<summary>returns the Name of an Object</summary>
   --<description>returns the Name of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Object_Name (Obj : in out Object'Class; Name : in Unbounded_String);
   --<summary>Sets the Name of an Object</summary>
   --<description>Sets the Name of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<parameter name="Name">The new Name of the Object</parameter>
   --<exception>None at this moment</exception>

   function Get_Object_Material (Obj : in Object) return Material_Ptr;
   --<summary>returns the Material of an Object</summary>
   --<description>returns the Material of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Object_Material (Obj : in out Object; Material : in Material_Ptr);
   --<summary>Sets the Material of an Object</summary>
   --<description>Sets the Material of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<parameter name="Material">The new Material of the Object</parameter>
   --<exception>None at this moment</exception>

   function Is_Primitive (Obj : in Object'Class) return Boolean;
   --<summary>Returns the value of Is_Primitive of the object</summary>
   --<description>Returns the value of Is_Primitive of the object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   function Is_CSG_Member (Obj : in Object'Class) return Boolean;
   --<summary>Returns True if the object is part of a CSG Tree</summary>
   --<description>Returns True if the object is part of a CSG Tree</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   procedure Invert_Flip_Normal (Obj : in out Object);
   --<summary>Inverts the Flip_Normal of an Object</summary>
   --<description>Inverts the Flip_Normal of an Object</description>
   --<parameter name="Obj">An Object</parameter>
   --<exception>None at this moment</exception>

   function Get_Flip_Normal (Obj : in Object'Class) return Boolean;
   --<summary>Returns the value of Flip_Normal of the object</summary>
   --<description>Returns the value of Flip_Normal of the object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   function Get_Object_Trans (Obj : in Object'Class) return Matrix_3D;
   --<summary>Returns the Trans Matrix of an Object</summary>
   --<description>Returns the Trans Matrix of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   function Get_Object_Trans_Inv (Obj : in Object'Class) return Matrix_3D;
   --<summary>Returns the Trans_Inv Matrix of an Object</summary>
   --<description>Returns the Trans_Inv Matrix of an Object</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   procedure Transform (Obj : in out Object; Mat : in Matrix_3D);
   --<summary>Applies Mat to the Transormation & Inverse Transformation Matrix of an Object</summary>
   --<description>Applies Mat to the Transormation & Inverse Transformation Matrix of an Object</summary>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<parameter name="Mat">The transformation matrix</parameter>
   --<exception>None at this moment</exception>

   function Evaluate_CSG (Obj : in out Object) return Boolean;
   --<summary>returns true if an Object intersection is actually resulting in a visible point</summary>
   --<description>returns true if an Object intersection is actually resulting in a visible point</description>
   --<parameter name="Obj">The object that is being hit</parameter>
   --<exception>None at this moment</exception>

   function Intersect (Obj : in Object; R : in Ray) return Natural;
   --<summary>Stub procedure to intersect an Object with a Ray</summary>
   --<description>Stub procedure to intersect an Object with a Ray. To be overridden by Object-specific versions</description>
   --<parameter name="Obj">An object of the Object Class Wide Type</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Obj : in Object; R : in Ray) return Natural;
   --<summary>Stub procedure to hit an Object with a Ray</summary>
   --<description>Stub procedure to hit an Object with a Ray. To be overridden by Object-specific versions</description>
   --<parameter name="Obj">An object of the Object Class Wide Type</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   procedure Print_Inside_Value (Obj : in Object; Level : in Large_Integer);

private

   type Object is tagged record
      Name : Unbounded_String := To_Unbounded_String ("NOT DEFINED");
      --  The Name of the Object
      Material : Material_Ptr := null;
      --  Pointer to the material of the object
      Trans : Matrix_3D;
      --  The transformation applied to this object
      Trans_Inv : Matrix_3D;
      --  The inverse of Trans
      Inside : Boolean := False;
      --  Are we currently inside the object?
      Flip_Normal : Boolean := False;
      --  Is this object flipping the normal (substracted from another object)?
      Is_Primitive : Boolean := True;
      --  Is this a primitive object?
      CSG_Parent : Object_Ptr := null;
      --  The Parent CSG Object this object is part of. Only one parent is possible.
   end record;

   type Object_List is record
      First : Object_List_Element_Ptr := null;
      --  Pointer to the first element
      No_Of_Objects : Large_Integer := 0;
      --  Static counter of elements in this list
      Current : Object_List_Element_Ptr := null;
      --  Pointer to current element, needed when processing through the list
   end record;

   type Object_List_Element is record
      Obj : Object_Ptr := null;
      --  Pointer to the object
      Next : Object_List_Element_Ptr := null;
      --  Pointer to the next element
   end record;

end Objects;
