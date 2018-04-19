with Ada.Text_IO; use Ada.Text_IO;

package body Project_View is

   procedure Make (Self : in out Scope; Packages : Pack_Vect.Vector) is
   begin
      Put_Line ("Making View object.");
      Self.Packages := Packages;
   end Make;

   overriding procedure Finalize (Self : in out Scope) is
   begin
      Put_Line ("Destroying View object.");
   end Finalize;

end Project_View;
