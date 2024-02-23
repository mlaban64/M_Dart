--<summary>The CSG-Objects package contains all Abstract Data Types that make up the 3D CSG Objects in a scene</summary>
--<description>The Objects package contains all Abstract Data Types that make up the 3D CSG Objects in a scene.
--</description>
--<group>Objects</group>

package Objects.CSG_Objects is
   ------------------
   -- ADT CSG_Object
   ------------------

   type CSG_Object is tagged private;
   --<summary>ADT for the CSG Object/summary>

   type CSG_Object_Ptr is access all CSG_Object'Class;
   --<summary>The access type to all CSG_Object'Class objects</summary>

   ---------------------------
   -- ADT CSG_Object Functions
   ---------------------------

   procedure Put (CSG_Tree : in CSG_Object; Msg : in String := "CSG Object ");
   --<summary>Prints a CSG_Object's statistics</summary>
   --<description>Prints a CSG_Object's statistics</description>
   --<parameter name="CSG_Tree">An object of the CSG_Object</parameter>
   --<exception>None at this moment</exception>

   function Construct_CSG_Object (Name : in String) return Object_Ptr;
   --<summary>Creates a CSG object</summary>
   --<description>Function that creates a CSG object</description>
   --<parameter name="Name">The Name of the Unit_Sphere</parameter>
   --<exception>None at this moment</exception>

   function CSG_Union (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr;
   --<summary>Creates a CSG Object that is the UNION of the left & right object</summary>
   --<description>Creates a CSG Object that is the UNION of the left & right object</description>
   --<parameter name="Left_Obj">The left object</parameter>
   --<parameter name="Right_Obj">The right object</parameter>
   --<parameter name="CSG_Obj_List">The list of CSG Objects any primitives need to be added to</parameter>
   --<exception>None at this moment</exception>

   function CSG_Difference (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr;
   --<summary>Creates a CSG Object that is the DIFFERENCE of the left & right object: LEFT - RIGHT</summary>
   --<description>Creates a CSG Object that is the DIFFERENCE of the left & right object: LEFT - RIGHT</description>
   --<parameter name="Left_Obj">The left object</parameter>
   --<parameter name="Right_Obj">The right object</parameter>
   --<parameter name="CSG_Obj_List">The list of CSG Objects any primitives need to be added to</parameter>
   --<exception>None at this moment</exception>

   function CSG_Intersection (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr;
   --<summary>Creates a CSG Object that is the INTERSECTION of the left & right object</summary>
   --<description>Creates a CSG Object that is the INTERSECTION of the left & right object</description>
   --<parameter name="Left_Obj">The left object</parameter>
   --<parameter name="Right_Obj">The right object</parameter>
   --<parameter name="CSG_Obj_List">The list of CSG Objects any primitives need to be added to</parameter>
   --<exception>None at this moment</exception>

   procedure Invert_Flip_Normal (Obj : in out CSG_Object);
   --<summary>Inverts the Flip_Normal of a CSG_Object recursively</summary>
   --<description>Inverts the Flip_Normal of a CSG_Object recursively</description>
   --<parameter name="Obj">An Object</parameter>
   --<exception>None at this moment</exception>

   function Get_Object_Material (Obj : in CSG_Object) return Material_Ptr;
   --<summary>returns the Material of a CSG_Object</summary>
   --<description>returns the Material of a CSG_Object, by traversing its children</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Object_Material (Obj : in out CSG_Object; Material : in Material_Ptr);
   --<summary>Sets the Material of a CSG Object</summary>
   --<description>Sets the Material of a CSG Object and its children</description>
   --<parameter name="Obj">A class-wide Object</parameter>
   --<parameter name="Material">The new Material of the Object</parameter>
   --<exception>None at this moment</exception>

   procedure Transform (Obj : in out CSG_Object; Mat : in Matrix_3D);
   --<summary>Applies Mat to the Transormation & Inverse Transformation Matrix of a CSG object</summary>
   --<description>Applies Mat to the Transormation & Inverse Transformation Matrix of a CSG object</summary>
   --<parameter name="Obj">A CSG Object</parameter>
   --<parameter name="Mat">The transformation matrix</parameter>
   --<exception>None at this moment</exception>

   function Evaluate_CSG (Obj : in out CSG_Object) return Boolean;
   --<summary>returns true if a CSG_Object is actually resulting in a visible point</summary>
   --<description>returns true if a CSG_Object is actually resulting in a visible point</description>
   --<parameter name="Obj">A CSG_Object that is being hit</parameter>
   --<exception>None at this moment</exception>

   procedure Print_Inside_Value(CSG_Obj: in CSG_Object; Level: in Large_Integer);

private

   type CSG_Operation is (UNION, DIFFERENCE, INTERSECTION);

   type CSG_Object is new Object with record
      Left_Obj, Right_Obj : Object_Ptr;
      --  the left & right object that make up the CSG object The Parent CSG Object this object is part of. Only one parent is
      --  possible
      Operation : CSG_Operation;
   end record;

end Objects.CSG_Objects;