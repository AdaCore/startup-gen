with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

--  The idea of this package is to provide an object that, when destroyed
--  will automatically de-register the attributes/packages with which it is
--  constructed. It is initialized using an aggregate.
package Project_View is

   --  For now, we only manage the lifetime of the added attributes,
   --  we dont have any accessors, hence the name Project_View

   type Attribute is
      record
         Name : Unbounded_String;
         Is_Indexed : Boolean := False;
         Is_List : Boolean := False;
      end record;

   package Attr_Vect is new Ada.Containers.Vectors
         (Index_Type => Natural,
          Element_Type => Attribute);

   type Project_Package is
      record
         Name : Unbounded_String;
         Attributes : Attr_Vect.Vector;
      end record;

   package Pack_Vect is new Ada.Containers.Vectors
         (Index_Type => Positive,
          Element_Type => Project_Package);

   --  Each package record contain its own attributes.
   type Scope is new Ada.Finalization.Controlled with
      record
         Packages : Pack_Vect.Vector;
      end record;

   --  Controlled procedures.
   overriding procedure Finalize (Self : in out Scope);

   --  Procedures.
   procedure Make (Self : in out Scope; Packages : Pack_Vect.Vector);

end Project_View;
