---------------------------------------------
--            A D C _ T E S T S            --
--                                         --
--  COMPLETAR CONFORME A EJERCICIOS 1 Y 2  --
--                                         --
---------------------------------------------
with BB, BB.ADC;
use BB, BB.ADC;
with ADC_Handler, ADC_Utils;
use ADC_Handler, ADC_Utils;
with CSV_Logs;          use CSV_Logs;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Real_Time;     use Ada.Real_Time;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

procedure ADC_Test is

   --------------------
   --  Polling_Test  --
   --------------------

   procedure Polling_Test is

      Conversion : ADC_Register := 0;  --  To store ADU_Count
      Pos_mm     : Position;           --  Position corresponding to Conversion
      Latency    : Duration;           --  Latency of conversion

      Clock_Initial : Time;            --  Used to measure latency of conversion
   begin

      Open_Log_Session (File_Name => "polling_test.csv");
      Log_Text ("ADC_Conv, Pos_mm, Latency");

      for I in 1 .. 100 loop

         -- Trigger conversion with interrupts disabled
         Write_CR (Get_CR (Trigger => True));

         -- Wait until the conversion is done by polling DR
         Clock_Initial := Clock;

         while not Is_Conversion_Complete (Conversion) loop
            Conversion := Read_DR;
         end loop;

         -- Calculate ADC latency
         Latency := To_Duration (Clock - Clock_Initial);

         -- Obtain first 12 MSBs
         Conversion := Read_DR and (2**12 - 1);

         -- Convert value from ADC to mm
         Pos_mm := ADC_To_Position (Conversion);

         -- Write the conversion result
         Log_Data ((Float (Conversion), Pos_mm, Float (Latency)));

      end loop;

      Close_Log_Session;

   end Polling_Test;

   ----------------------
   --  Interrupt_Test  --
   ----------------------

   procedure Interrupt_Test is

      Conversion : ADC_Register;  --  To store ADU_Count
      Pos_mm     : Position;      --  Position corresponding to Conversion
      Latency    : Duration;      --  Latency of conversion

       Clock_Initial : Time;            --  Used to measure latency of conversion

   begin
      Open_Log_Session ("interrupt_test.csv");
      Log_Text ("ADC_Conv, Pos_mm, Latency");

      -- Register interrupt handler
      Attach_ADC_Handler(EOC_Handler'Access);

      for I in 1 .. 100 loop

         -- Trigger conversion with interrupts enabled
         Set_False(End_Of_Conversion);
         Write_CR (Get_CR (Trigger => True, Interrupt_Enable => True));
         Clock_Initial := Clock;

         -- Wait for EOC Suspension Object
         Suspend_Until_True(End_Of_Conversion);

         -- Calculate ADC latency
         Latency := To_Duration (Clock - Clock_Initial);

         -- Obtain first 12 MSBs
         Conversion := Read_DR and (2**12 - 1);

         -- Convert value from ADC to mm
         Pos_mm := ADC_To_Position (Conversion);

         -- Write the conversion result
         Log_Data ((Float (Conversion), Pos_mm, Float (Latency)));

      end loop;

      Close_Log_Session;

   end Interrupt_Test;

   --------------------
   --  Main Program  --
   --------------------

begin

   Put_Line ("POLLING Test");
   Polling_Test;

   Put_Line ("INTERRUPT Test");
   Interrupt_Test;

   Put_Line ("End of ADC Test");

end ADC_Test;
