with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package body Test is

   SRAM_Data : array (1 .. 10) of Unsigned_8 := (others => 42)
     with Linker_Section => ".sram_data";

   SRAM_BSS : array (1 .. 10) of Unsigned_8 := (others => 0)
     with Linker_Section => ".sram_bss";

   SRAM_Heap_Start : Unsigned_32;
   pragma Import (C, SRAM_Heap_Start, "__sram_heap_start");

   SRAM_Heap_End : Unsigned_32;
   pragma Import (C, SRAM_Heap_End, "__sram_heap_end");

   CCM_Data : array (1 .. 10) of Unsigned_8 := (others => 42);
   CCM_BSS  : array (1 .. 10) of Unsigned_8 := (others => 0);

   Heap_Start : Unsigned_32;
   pragma Import (C, Heap_Start, "__heap_start");

   Heap_End : Unsigned_32;
   pragma Import (C, Heap_End, "__heap_end");

   --------------------
   -- Check_Memories --
   --------------------

   procedure Check_Memories is
      Result : Boolean := True;
   begin
      Put_Line ("Boot memory: CCM");

      if not In_CCM (CCM_Data'Address) then
         Put_Line ("Wrong address for CCM_Data");
         Result := False;
      end if;

      if not In_CCM (CCM_BSS'Address) then
         Put_Line ("Wrong address for CCM_Data");
         Result := False;
      end if;

      if not In_SRAM (SRAM_Heap_Start'Address) then
         Put_Line ("Wrong address for CCM_Heap_Start");
         Result := False;
      end if;

      if not In_SRAM (SRAM_Heap_End'Address) then
         Put_Line ("Wrong address for CCM_Heap_End");
         Result := False;
      end if;

      if not In_SRAM (SRAM_Data'Address) then
         Put_Line ("Wrong address for SRAM_Data");
         Result := False;
      end if;

      if not In_SRAM (SRAM_BSS'Address) then
         Put_Line ("Wrong address for SRAM_Data");
         Result := False;
      end if;

      if not In_CCM (Heap_Start'Address) then
         Put_Line ("Wrong address for Heap_Start");
         Result := False;
      end if;

      if not In_CCM (Heap_End'Address) then
         Put_Line ("Wrong address for Heap_End");
         Result := False;
      end if;

      --  Check that the bss was cleared
      for Elt of SRAM_BSS loop
         if Elt /= 0 then
            Put_Line ("Wrong value for SRAM_BSS");
            Result := False;
         end if;
      end loop;
      for Elt of CCM_BSS loop
         if Elt /= 0 then
            Put_Line ("Wrong value for CCM_BSS");
            Result := False;
         end if;
      end loop;

      if Result then
         Put_Line ("PASS: Memories checks");
      else
         Put_Line ("FAIL: Memories checks");
      end if;
   end Check_Memories;

end Test;
