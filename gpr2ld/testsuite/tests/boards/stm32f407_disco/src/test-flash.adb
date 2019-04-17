with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package body Test is

   CCM_Data : array (1 .. 10) of Unsigned_8 := (others => 42)
     with Linker_Section => ".ccm_data";
   CCM_BSS : array (1 .. 10) of Unsigned_8 := (others => 0)
     with Linker_Section => ".ccm_bss";

   CCM_Heap_Start : Unsigned_32;
   pragma Import (C, CCM_Heap_Start, "__ccm_heap_start");

   CCM_Heap_End : Unsigned_32;
   pragma Import (C, CCM_Heap_End, "__ccm_heap_end");

   SRAM_Data : array (1 .. 10) of Unsigned_8 := (others => 42);
   SRAM_BSS : array (1 .. 10) of Unsigned_8 := (others => 0);

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
      Put_Line ("Boot memory: FLASH");

      if not In_CCM (CCM_Data'Address) then
         Put_Line ("Wrong address for CCM_Data");
         Result := False;
      end if;

      if not In_CCM (CCM_BSS'Address) then
         Put_Line ("Wrong address for CCM_Data");
         Result := False;
      end if;

      if not In_CCM (CCM_Heap_Start'Address) then
         Put_Line ("Wrong address for CCM_Heap_Start");
         Result := False;
      end if;

      if not In_CCM (CCM_Heap_End'Address) then
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

      if not In_SRAM (Heap_Start'Address) then
         Put_Line ("Wrong address for Heap_Start");
         Result := False;
      end if;

      if not In_SRAM (Heap_End'Address) then
         Put_Line ("Wrong address for Heap_End");
         Result := False;
      end if;

      --  Check that the data was copied from flash
      for Elt of SRAM_Data loop
         if Elt /= 42 then
            Put_Line ("Wrong value for SRAM_Data");
            Result := False;
         end if;
      end loop;
      for Elt of CCM_Data loop
         if Elt /= 42 then
            Put_Line ("Wrong value for CCM_Data");
            Result := False;
         end if;
      end loop;

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
