module BytePrinter(
    input clk,
    input reset,
    input in_valid,
    input[7:0] in_byte
);

`ifdef VCS

integer file;

initial begin
    file = $fopen("byte_printer.txt", "w");
    if (file == 0) begin
        $display("Failed to open byte_printer.txt");
        $finish;
    end
end

always @(posedge clk) begin
    if (in_valid & ~reset) begin
        $fwrite(file, "%c", in_byte);
    end
end 

final begin
    $fclose(file);
end

`endif

endmodule
