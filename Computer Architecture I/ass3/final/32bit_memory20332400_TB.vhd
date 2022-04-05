LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY memory_512_tb IS
END memory_512_tb;

ARCHITECTURE Behavioral OF memory_512_tb IS
    COMPONENT memory_512
        PORT (
            mem_address : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            write_enable, clk : IN STD_LOGIC;
            data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL mem_address, data_in : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL write_enable, clk : STD_LOGIC := '0';

    -- output
    SIGNAL data_out : STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN
    uut : memory_512
    PORT MAP(
        mem_address => mem_address,
        data_in => data_in,
        write_enable => write_enable,
        clk => clk,
        data_out => data_out
    );

    clk <= NOT clk AFTER 1ns;
    stim_proc : PROCESS
    BEGIN

        -- read from all memory addresses
        write_enable <= '0';

        mem_address <= x"00000000"; -- 0
        WAIT FOR 1 ns;
        mem_address <= x"00000001"; -- 1
        WAIT FOR 1 ns;
        mem_address <= x"00000002"; -- 2
        WAIT FOR 1 ns;
        mem_address <= x"00000003"; -- 3
        WAIT FOR 1 ns;
        mem_address <= x"00000004"; -- 4
        WAIT FOR 1 ns;
        mem_address <= x"00000005"; -- 5
        WAIT FOR 1 ns;
        mem_address <= x"00000006"; -- 6
        WAIT FOR 1 ns;
        mem_address <= x"00000007"; -- 7
        WAIT FOR 1 ns;
        mem_address <= x"00000008"; -- 8
        WAIT FOR 1 ns;
        mem_address <= x"00000009"; -- 9
        WAIT FOR 1 ns;
        mem_address <= x"0000000a"; -- 10
        WAIT FOR 1 ns;
        mem_address <= x"0000000b"; -- 11
        WAIT FOR 1 ns;
        mem_address <= x"0000000c"; -- 12
        WAIT FOR 1 ns;
        mem_address <= x"0000000d"; -- 13
        WAIT FOR 1 ns;
        mem_address <= x"0000000e"; -- 14
        WAIT FOR 1 ns;
        mem_address <= x"0000000f"; -- 15
        WAIT FOR 1 ns;
        mem_address <= x"00000010"; -- 16
        WAIT FOR 1 ns;
        mem_address <= x"00000011"; -- 17
        WAIT FOR 1 ns;
        mem_address <= x"00000012"; -- 18
        WAIT FOR 1 ns;
        mem_address <= x"00000013"; -- 19
        WAIT FOR 1 ns;
        mem_address <= x"00000014"; -- 20
        WAIT FOR 1 ns;
        mem_address <= x"00000015"; -- 21
        WAIT FOR 1 ns;
        mem_address <= x"00000016"; -- 22
        WAIT FOR 1 ns;
        mem_address <= x"00000017"; -- 23
        WAIT FOR 1 ns;
        mem_address <= x"00000018"; -- 24
        WAIT FOR 1 ns;
        mem_address <= x"00000019"; -- 25
        WAIT FOR 1 ns;
        mem_address <= x"0000001a"; -- 26
        WAIT FOR 1 ns;
        mem_address <= x"0000001b"; -- 27
        WAIT FOR 1 ns;
        mem_address <= x"0000001c"; -- 28
        WAIT FOR 1 ns;
        mem_address <= x"0000001d"; -- 29
        WAIT FOR 1 ns;
        mem_address <= x"0000001e"; -- 30
        WAIT FOR 1 ns;
        mem_address <= x"0000001f"; -- 31
        WAIT FOR 1 ns;
        mem_address <= x"00000020"; -- 32
        WAIT FOR 1 ns;
        mem_address <= x"00000021"; -- 33
        WAIT FOR 1 ns;
        mem_address <= x"00000022"; -- 34
        WAIT FOR 1 ns;
        mem_address <= x"00000023"; -- 35
        WAIT FOR 1 ns;
        mem_address <= x"00000024"; -- 36
        WAIT FOR 1 ns;
        mem_address <= x"00000025"; -- 37
        WAIT FOR 1 ns;
        mem_address <= x"00000026"; -- 38
        WAIT FOR 1 ns;
        mem_address <= x"00000027"; -- 39
        WAIT FOR 1 ns;
        mem_address <= x"00000028"; -- 40
        WAIT FOR 1 ns;
        mem_address <= x"00000029"; -- 41
        WAIT FOR 1 ns;
        mem_address <= x"0000002a"; -- 42
        WAIT FOR 1 ns;
        mem_address <= x"0000002b"; -- 43
        WAIT FOR 1 ns;
        mem_address <= x"0000002c"; -- 44
        WAIT FOR 1 ns;
        mem_address <= x"0000002d"; -- 45
        WAIT FOR 1 ns;
        mem_address <= x"0000002e"; -- 46
        WAIT FOR 1 ns;
        mem_address <= x"0000002f"; -- 47
        WAIT FOR 1 ns;
        mem_address <= x"00000030"; -- 48
        WAIT FOR 1 ns;
        mem_address <= x"00000031"; -- 49
        WAIT FOR 1 ns;
        mem_address <= x"00000032"; -- 50
        WAIT FOR 1 ns;
        mem_address <= x"00000033"; -- 51
        WAIT FOR 1 ns;
        mem_address <= x"00000034"; -- 52
        WAIT FOR 1 ns;
        mem_address <= x"00000035"; -- 53
        WAIT FOR 1 ns;
        mem_address <= x"00000036"; -- 54
        WAIT FOR 1 ns;
        mem_address <= x"00000037"; -- 55
        WAIT FOR 1 ns;
        mem_address <= x"00000038"; -- 56
        WAIT FOR 1 ns;
        mem_address <= x"00000039"; -- 57
        WAIT FOR 1 ns;
        mem_address <= x"0000003a"; -- 58
        WAIT FOR 1 ns;
        mem_address <= x"0000003b"; -- 59
        WAIT FOR 1 ns;
        mem_address <= x"0000003c"; -- 60
        WAIT FOR 1 ns;
        mem_address <= x"0000003d"; -- 61
        WAIT FOR 1 ns;
        mem_address <= x"0000003e"; -- 62
        WAIT FOR 1 ns;
        mem_address <= x"0000003f"; -- 63
        WAIT FOR 1 ns;
        mem_address <= x"00000040"; -- 64
        WAIT FOR 1 ns;
        mem_address <= x"00000041"; -- 65
        WAIT FOR 1 ns;
        mem_address <= x"00000042"; -- 66
        WAIT FOR 1 ns;
        mem_address <= x"00000043"; -- 67
        WAIT FOR 1 ns;
        mem_address <= x"00000044"; -- 68
        WAIT FOR 1 ns;
        mem_address <= x"00000045"; -- 69
        WAIT FOR 1 ns;
        mem_address <= x"00000046"; -- 70
        WAIT FOR 1 ns;
        mem_address <= x"00000047"; -- 71
        WAIT FOR 1 ns;
        mem_address <= x"00000048"; -- 72
        WAIT FOR 1 ns;
        mem_address <= x"00000049"; -- 73
        WAIT FOR 1 ns;
        mem_address <= x"0000004a"; -- 74
        WAIT FOR 1 ns;
        mem_address <= x"0000004b"; -- 75
        WAIT FOR 1 ns;
        mem_address <= x"0000004c"; -- 76
        WAIT FOR 1 ns;
        mem_address <= x"0000004d"; -- 77
        WAIT FOR 1 ns;
        mem_address <= x"0000004e"; -- 78
        WAIT FOR 1 ns;
        mem_address <= x"0000004f"; -- 79
        WAIT FOR 1 ns;
        mem_address <= x"00000050"; -- 80
        WAIT FOR 1 ns;
        mem_address <= x"00000051"; -- 81
        WAIT FOR 1 ns;
        mem_address <= x"00000052"; -- 82
        WAIT FOR 1 ns;
        mem_address <= x"00000053"; -- 83
        WAIT FOR 1 ns;
        mem_address <= x"00000054"; -- 84
        WAIT FOR 1 ns;
        mem_address <= x"00000055"; -- 85
        WAIT FOR 1 ns;
        mem_address <= x"00000056"; -- 86
        WAIT FOR 1 ns;
        mem_address <= x"00000057"; -- 87
        WAIT FOR 1 ns;
        mem_address <= x"00000058"; -- 88
        WAIT FOR 1 ns;
        mem_address <= x"00000059"; -- 89
        WAIT FOR 1 ns;
        mem_address <= x"0000005a"; -- 90
        WAIT FOR 1 ns;
        mem_address <= x"0000005b"; -- 91
        WAIT FOR 1 ns;
        mem_address <= x"0000005c"; -- 92
        WAIT FOR 1 ns;
        mem_address <= x"0000005d"; -- 93
        WAIT FOR 1 ns;
        mem_address <= x"0000005e"; -- 94
        WAIT FOR 1 ns;
        mem_address <= x"0000005f"; -- 95
        WAIT FOR 1 ns;
        mem_address <= x"00000060"; -- 96
        WAIT FOR 1 ns;
        mem_address <= x"00000061"; -- 97
        WAIT FOR 1 ns;
        mem_address <= x"00000062"; -- 98
        WAIT FOR 1 ns;
        mem_address <= x"00000063"; -- 99
        WAIT FOR 1 ns;
        mem_address <= x"00000064"; -- 100
        WAIT FOR 1 ns;
        mem_address <= x"00000065"; -- 101
        WAIT FOR 1 ns;
        mem_address <= x"00000066"; -- 102
        WAIT FOR 1 ns;
        mem_address <= x"00000067"; -- 103
        WAIT FOR 1 ns;
        mem_address <= x"00000068"; -- 104
        WAIT FOR 1 ns;
        mem_address <= x"00000069"; -- 105
        WAIT FOR 1 ns;
        mem_address <= x"0000006a"; -- 106
        WAIT FOR 1 ns;
        mem_address <= x"0000006b"; -- 107
        WAIT FOR 1 ns;
        mem_address <= x"0000006c"; -- 108
        WAIT FOR 1 ns;
        mem_address <= x"0000006d"; -- 109
        WAIT FOR 1 ns;
        mem_address <= x"0000006e"; -- 110
        WAIT FOR 1 ns;
        mem_address <= x"0000006f"; -- 111
        WAIT FOR 1 ns;
        mem_address <= x"00000070"; -- 112
        WAIT FOR 1 ns;
        mem_address <= x"00000071"; -- 113
        WAIT FOR 1 ns;
        mem_address <= x"00000072"; -- 114
        WAIT FOR 1 ns;
        mem_address <= x"00000073"; -- 115
        WAIT FOR 1 ns;
        mem_address <= x"00000074"; -- 116
        WAIT FOR 1 ns;
        mem_address <= x"00000075"; -- 117
        WAIT FOR 1 ns;
        mem_address <= x"00000076"; -- 118
        WAIT FOR 1 ns;
        mem_address <= x"00000077"; -- 119
        WAIT FOR 1 ns;
        mem_address <= x"00000078"; -- 120
        WAIT FOR 1 ns;
        mem_address <= x"00000079"; -- 121
        WAIT FOR 1 ns;
        mem_address <= x"0000007a"; -- 122
        WAIT FOR 1 ns;
        mem_address <= x"0000007b"; -- 123
        WAIT FOR 1 ns;
        mem_address <= x"0000007c"; -- 124
        WAIT FOR 1 ns;
        mem_address <= x"0000007d"; -- 125
        WAIT FOR 1 ns;
        mem_address <= x"0000007e"; -- 126
        WAIT FOR 1 ns;
        mem_address <= x"0000007f"; -- 127
        WAIT FOR 1 ns;
        mem_address <= x"00000080"; -- 128
        WAIT FOR 1 ns;
        mem_address <= x"00000081"; -- 129
        WAIT FOR 1 ns;
        mem_address <= x"00000082"; -- 130
        WAIT FOR 1 ns;
        mem_address <= x"00000083"; -- 131
        WAIT FOR 1 ns;
        mem_address <= x"00000084"; -- 132
        WAIT FOR 1 ns;
        mem_address <= x"00000085"; -- 133
        WAIT FOR 1 ns;
        mem_address <= x"00000086"; -- 134
        WAIT FOR 1 ns;
        mem_address <= x"00000087"; -- 135
        WAIT FOR 1 ns;
        mem_address <= x"00000088"; -- 136
        WAIT FOR 1 ns;
        mem_address <= x"00000089"; -- 137
        WAIT FOR 1 ns;
        mem_address <= x"0000008a"; -- 138
        WAIT FOR 1 ns;
        mem_address <= x"0000008b"; -- 139
        WAIT FOR 1 ns;
        mem_address <= x"0000008c"; -- 140
        WAIT FOR 1 ns;
        mem_address <= x"0000008d"; -- 141
        WAIT FOR 1 ns;
        mem_address <= x"0000008e"; -- 142
        WAIT FOR 1 ns;
        mem_address <= x"0000008f"; -- 143
        WAIT FOR 1 ns;
        mem_address <= x"00000090"; -- 144
        WAIT FOR 1 ns;
        mem_address <= x"00000091"; -- 145
        WAIT FOR 1 ns;
        mem_address <= x"00000092"; -- 146
        WAIT FOR 1 ns;
        mem_address <= x"00000093"; -- 147
        WAIT FOR 1 ns;
        mem_address <= x"00000094"; -- 148
        WAIT FOR 1 ns;
        mem_address <= x"00000095"; -- 149
        WAIT FOR 1 ns;
        mem_address <= x"00000096"; -- 150
        WAIT FOR 1 ns;
        mem_address <= x"00000097"; -- 151
        WAIT FOR 1 ns;
        mem_address <= x"00000098"; -- 152
        WAIT FOR 1 ns;
        mem_address <= x"00000099"; -- 153
        WAIT FOR 1 ns;
        mem_address <= x"0000009a"; -- 154
        WAIT FOR 1 ns;
        mem_address <= x"0000009b"; -- 155
        WAIT FOR 1 ns;
        mem_address <= x"0000009c"; -- 156
        WAIT FOR 1 ns;
        mem_address <= x"0000009d"; -- 157
        WAIT FOR 1 ns;
        mem_address <= x"0000009e"; -- 158
        WAIT FOR 1 ns;
        mem_address <= x"0000009f"; -- 159
        WAIT FOR 1 ns;
        mem_address <= x"000000a0"; -- 160
        WAIT FOR 1 ns;
        mem_address <= x"000000a1"; -- 161
        WAIT FOR 1 ns;
        mem_address <= x"000000a2"; -- 162
        WAIT FOR 1 ns;
        mem_address <= x"000000a3"; -- 163
        WAIT FOR 1 ns;
        mem_address <= x"000000a4"; -- 164
        WAIT FOR 1 ns;
        mem_address <= x"000000a5"; -- 165
        WAIT FOR 1 ns;
        mem_address <= x"000000a6"; -- 166
        WAIT FOR 1 ns;
        mem_address <= x"000000a7"; -- 167
        WAIT FOR 1 ns;
        mem_address <= x"000000a8"; -- 168
        WAIT FOR 1 ns;
        mem_address <= x"000000a9"; -- 169
        WAIT FOR 1 ns;
        mem_address <= x"000000aa"; -- 170
        WAIT FOR 1 ns;
        mem_address <= x"000000ab"; -- 171
        WAIT FOR 1 ns;
        mem_address <= x"000000ac"; -- 172
        WAIT FOR 1 ns;
        mem_address <= x"000000ad"; -- 173
        WAIT FOR 1 ns;
        mem_address <= x"000000ae"; -- 174
        WAIT FOR 1 ns;
        mem_address <= x"000000af"; -- 175
        WAIT FOR 1 ns;
        mem_address <= x"000000b0"; -- 176
        WAIT FOR 1 ns;
        mem_address <= x"000000b1"; -- 177
        WAIT FOR 1 ns;
        mem_address <= x"000000b2"; -- 178
        WAIT FOR 1 ns;
        mem_address <= x"000000b3"; -- 179
        WAIT FOR 1 ns;
        mem_address <= x"000000b4"; -- 180
        WAIT FOR 1 ns;
        mem_address <= x"000000b5"; -- 181
        WAIT FOR 1 ns;
        mem_address <= x"000000b6"; -- 182
        WAIT FOR 1 ns;
        mem_address <= x"000000b7"; -- 183
        WAIT FOR 1 ns;
        mem_address <= x"000000b8"; -- 184
        WAIT FOR 1 ns;
        mem_address <= x"000000b9"; -- 185
        WAIT FOR 1 ns;
        mem_address <= x"000000ba"; -- 186
        WAIT FOR 1 ns;
        mem_address <= x"000000bb"; -- 187
        WAIT FOR 1 ns;
        mem_address <= x"000000bc"; -- 188
        WAIT FOR 1 ns;
        mem_address <= x"000000bd"; -- 189
        WAIT FOR 1 ns;
        mem_address <= x"000000be"; -- 190
        WAIT FOR 1 ns;
        mem_address <= x"000000bf"; -- 191
        WAIT FOR 1 ns;
        mem_address <= x"000000c0"; -- 192
        WAIT FOR 1 ns;
        mem_address <= x"000000c1"; -- 193
        WAIT FOR 1 ns;
        mem_address <= x"000000c2"; -- 194
        WAIT FOR 1 ns;
        mem_address <= x"000000c3"; -- 195
        WAIT FOR 1 ns;
        mem_address <= x"000000c4"; -- 196
        WAIT FOR 1 ns;
        mem_address <= x"000000c5"; -- 197
        WAIT FOR 1 ns;
        mem_address <= x"000000c6"; -- 198
        WAIT FOR 1 ns;
        mem_address <= x"000000c7"; -- 199
        WAIT FOR 1 ns;
        mem_address <= x"000000c8"; -- 200
        WAIT FOR 1 ns;
        mem_address <= x"000000c9"; -- 201
        WAIT FOR 1 ns;
        mem_address <= x"000000ca"; -- 202
        WAIT FOR 1 ns;
        mem_address <= x"000000cb"; -- 203
        WAIT FOR 1 ns;
        mem_address <= x"000000cc"; -- 204
        WAIT FOR 1 ns;
        mem_address <= x"000000cd"; -- 205
        WAIT FOR 1 ns;
        mem_address <= x"000000ce"; -- 206
        WAIT FOR 1 ns;
        mem_address <= x"000000cf"; -- 207
        WAIT FOR 1 ns;
        mem_address <= x"000000d0"; -- 208
        WAIT FOR 1 ns;
        mem_address <= x"000000d1"; -- 209
        WAIT FOR 1 ns;
        mem_address <= x"000000d2"; -- 210
        WAIT FOR 1 ns;
        mem_address <= x"000000d3"; -- 211
        WAIT FOR 1 ns;
        mem_address <= x"000000d4"; -- 212
        WAIT FOR 1 ns;
        mem_address <= x"000000d5"; -- 213
        WAIT FOR 1 ns;
        mem_address <= x"000000d6"; -- 214
        WAIT FOR 1 ns;
        mem_address <= x"000000d7"; -- 215
        WAIT FOR 1 ns;
        mem_address <= x"000000d8"; -- 216
        WAIT FOR 1 ns;
        mem_address <= x"000000d9"; -- 217
        WAIT FOR 1 ns;
        mem_address <= x"000000da"; -- 218
        WAIT FOR 1 ns;
        mem_address <= x"000000db"; -- 219
        WAIT FOR 1 ns;
        mem_address <= x"000000dc"; -- 220
        WAIT FOR 1 ns;
        mem_address <= x"000000dd"; -- 221
        WAIT FOR 1 ns;
        mem_address <= x"000000de"; -- 222
        WAIT FOR 1 ns;
        mem_address <= x"000000df"; -- 223
        WAIT FOR 1 ns;
        mem_address <= x"000000e0"; -- 224
        WAIT FOR 1 ns;
        mem_address <= x"000000e1"; -- 225
        WAIT FOR 1 ns;
        mem_address <= x"000000e2"; -- 226
        WAIT FOR 1 ns;
        mem_address <= x"000000e3"; -- 227
        WAIT FOR 1 ns;
        mem_address <= x"000000e4"; -- 228
        WAIT FOR 1 ns;
        mem_address <= x"000000e5"; -- 229
        WAIT FOR 1 ns;
        mem_address <= x"000000e6"; -- 230
        WAIT FOR 1 ns;
        mem_address <= x"000000e7"; -- 231
        WAIT FOR 1 ns;
        mem_address <= x"000000e8"; -- 232
        WAIT FOR 1 ns;
        mem_address <= x"000000e9"; -- 233
        WAIT FOR 1 ns;
        mem_address <= x"000000ea"; -- 234
        WAIT FOR 1 ns;
        mem_address <= x"000000eb"; -- 235
        WAIT FOR 1 ns;
        mem_address <= x"000000ec"; -- 236
        WAIT FOR 1 ns;
        mem_address <= x"000000ed"; -- 237
        WAIT FOR 1 ns;
        mem_address <= x"000000ee"; -- 238
        WAIT FOR 1 ns;
        mem_address <= x"000000ef"; -- 239
        WAIT FOR 1 ns;
        mem_address <= x"000000f0"; -- 240
        WAIT FOR 1 ns;
        mem_address <= x"000000f1"; -- 241
        WAIT FOR 1 ns;
        mem_address <= x"000000f2"; -- 242
        WAIT FOR 1 ns;
        mem_address <= x"000000f3"; -- 243
        WAIT FOR 1 ns;
        mem_address <= x"000000f4"; -- 244
        WAIT FOR 1 ns;
        mem_address <= x"000000f5"; -- 245
        WAIT FOR 1 ns;
        mem_address <= x"000000f6"; -- 246
        WAIT FOR 1 ns;
        mem_address <= x"000000f7"; -- 247
        WAIT FOR 1 ns;
        mem_address <= x"000000f8"; -- 248
        WAIT FOR 1 ns;
        mem_address <= x"000000f9"; -- 249
        WAIT FOR 1 ns;
        mem_address <= x"000000fa"; -- 250
        WAIT FOR 1 ns;
        mem_address <= x"000000fb"; -- 251
        WAIT FOR 1 ns;
        mem_address <= x"000000fc"; -- 252
        WAIT FOR 1 ns;
        mem_address <= x"000000fd"; -- 253
        WAIT FOR 1 ns;
        mem_address <= x"000000fe"; -- 254
        WAIT FOR 1 ns;
        mem_address <= x"000000ff"; -- 255
        WAIT FOR 1 ns;
        mem_address <= x"00000100"; -- 256
        WAIT FOR 1 ns;
        mem_address <= x"00000101"; -- 257
        WAIT FOR 1 ns;
        mem_address <= x"00000102"; -- 258
        WAIT FOR 1 ns;
        mem_address <= x"00000103"; -- 259
        WAIT FOR 1 ns;
        mem_address <= x"00000104"; -- 260
        WAIT FOR 1 ns;
        mem_address <= x"00000105"; -- 261
        WAIT FOR 1 ns;
        mem_address <= x"00000106"; -- 262
        WAIT FOR 1 ns;
        mem_address <= x"00000107"; -- 263
        WAIT FOR 1 ns;
        mem_address <= x"00000108"; -- 264
        WAIT FOR 1 ns;
        mem_address <= x"00000109"; -- 265
        WAIT FOR 1 ns;
        mem_address <= x"0000010a"; -- 266
        WAIT FOR 1 ns;
        mem_address <= x"0000010b"; -- 267
        WAIT FOR 1 ns;
        mem_address <= x"0000010c"; -- 268
        WAIT FOR 1 ns;
        mem_address <= x"0000010d"; -- 269
        WAIT FOR 1 ns;
        mem_address <= x"0000010e"; -- 270
        WAIT FOR 1 ns;
        mem_address <= x"0000010f"; -- 271
        WAIT FOR 1 ns;
        mem_address <= x"00000110"; -- 272
        WAIT FOR 1 ns;
        mem_address <= x"00000111"; -- 273
        WAIT FOR 1 ns;
        mem_address <= x"00000112"; -- 274
        WAIT FOR 1 ns;
        mem_address <= x"00000113"; -- 275
        WAIT FOR 1 ns;
        mem_address <= x"00000114"; -- 276
        WAIT FOR 1 ns;
        mem_address <= x"00000115"; -- 277
        WAIT FOR 1 ns;
        mem_address <= x"00000116"; -- 278
        WAIT FOR 1 ns;
        mem_address <= x"00000117"; -- 279
        WAIT FOR 1 ns;
        mem_address <= x"00000118"; -- 280
        WAIT FOR 1 ns;
        mem_address <= x"00000119"; -- 281
        WAIT FOR 1 ns;
        mem_address <= x"0000011a"; -- 282
        WAIT FOR 1 ns;
        mem_address <= x"0000011b"; -- 283
        WAIT FOR 1 ns;
        mem_address <= x"0000011c"; -- 284
        WAIT FOR 1 ns;
        mem_address <= x"0000011d"; -- 285
        WAIT FOR 1 ns;
        mem_address <= x"0000011e"; -- 286
        WAIT FOR 1 ns;
        mem_address <= x"0000011f"; -- 287
        WAIT FOR 1 ns;
        mem_address <= x"00000120"; -- 288
        WAIT FOR 1 ns;
        mem_address <= x"00000121"; -- 289
        WAIT FOR 1 ns;
        mem_address <= x"00000122"; -- 290
        WAIT FOR 1 ns;
        mem_address <= x"00000123"; -- 291
        WAIT FOR 1 ns;
        mem_address <= x"00000124"; -- 292
        WAIT FOR 1 ns;
        mem_address <= x"00000125"; -- 293
        WAIT FOR 1 ns;
        mem_address <= x"00000126"; -- 294
        WAIT FOR 1 ns;
        mem_address <= x"00000127"; -- 295
        WAIT FOR 1 ns;
        mem_address <= x"00000128"; -- 296
        WAIT FOR 1 ns;
        mem_address <= x"00000129"; -- 297
        WAIT FOR 1 ns;
        mem_address <= x"0000012a"; -- 298
        WAIT FOR 1 ns;
        mem_address <= x"0000012b"; -- 299
        WAIT FOR 1 ns;
        mem_address <= x"0000012c"; -- 300
        WAIT FOR 1 ns;
        mem_address <= x"0000012d"; -- 301
        WAIT FOR 1 ns;
        mem_address <= x"0000012e"; -- 302
        WAIT FOR 1 ns;
        mem_address <= x"0000012f"; -- 303
        WAIT FOR 1 ns;
        mem_address <= x"00000130"; -- 304
        WAIT FOR 1 ns;
        mem_address <= x"00000131"; -- 305
        WAIT FOR 1 ns;
        mem_address <= x"00000132"; -- 306
        WAIT FOR 1 ns;
        mem_address <= x"00000133"; -- 307
        WAIT FOR 1 ns;
        mem_address <= x"00000134"; -- 308
        WAIT FOR 1 ns;
        mem_address <= x"00000135"; -- 309
        WAIT FOR 1 ns;
        mem_address <= x"00000136"; -- 310
        WAIT FOR 1 ns;
        mem_address <= x"00000137"; -- 311
        WAIT FOR 1 ns;
        mem_address <= x"00000138"; -- 312
        WAIT FOR 1 ns;
        mem_address <= x"00000139"; -- 313
        WAIT FOR 1 ns;
        mem_address <= x"0000013a"; -- 314
        WAIT FOR 1 ns;
        mem_address <= x"0000013b"; -- 315
        WAIT FOR 1 ns;
        mem_address <= x"0000013c"; -- 316
        WAIT FOR 1 ns;
        mem_address <= x"0000013d"; -- 317
        WAIT FOR 1 ns;
        mem_address <= x"0000013e"; -- 318
        WAIT FOR 1 ns;
        mem_address <= x"0000013f"; -- 319
        WAIT FOR 1 ns;
        mem_address <= x"00000140"; -- 320
        WAIT FOR 1 ns;
        mem_address <= x"00000141"; -- 321
        WAIT FOR 1 ns;
        mem_address <= x"00000142"; -- 322
        WAIT FOR 1 ns;
        mem_address <= x"00000143"; -- 323
        WAIT FOR 1 ns;
        mem_address <= x"00000144"; -- 324
        WAIT FOR 1 ns;
        mem_address <= x"00000145"; -- 325
        WAIT FOR 1 ns;
        mem_address <= x"00000146"; -- 326
        WAIT FOR 1 ns;
        mem_address <= x"00000147"; -- 327
        WAIT FOR 1 ns;
        mem_address <= x"00000148"; -- 328
        WAIT FOR 1 ns;
        mem_address <= x"00000149"; -- 329
        WAIT FOR 1 ns;
        mem_address <= x"0000014a"; -- 330
        WAIT FOR 1 ns;
        mem_address <= x"0000014b"; -- 331
        WAIT FOR 1 ns;
        mem_address <= x"0000014c"; -- 332
        WAIT FOR 1 ns;
        mem_address <= x"0000014d"; -- 333
        WAIT FOR 1 ns;
        mem_address <= x"0000014e"; -- 334
        WAIT FOR 1 ns;
        mem_address <= x"0000014f"; -- 335
        WAIT FOR 1 ns;
        mem_address <= x"00000150"; -- 336
        WAIT FOR 1 ns;
        mem_address <= x"00000151"; -- 337
        WAIT FOR 1 ns;
        mem_address <= x"00000152"; -- 338
        WAIT FOR 1 ns;
        mem_address <= x"00000153"; -- 339
        WAIT FOR 1 ns;
        mem_address <= x"00000154"; -- 340
        WAIT FOR 1 ns;
        mem_address <= x"00000155"; -- 341
        WAIT FOR 1 ns;
        mem_address <= x"00000156"; -- 342
        WAIT FOR 1 ns;
        mem_address <= x"00000157"; -- 343
        WAIT FOR 1 ns;
        mem_address <= x"00000158"; -- 344
        WAIT FOR 1 ns;
        mem_address <= x"00000159"; -- 345
        WAIT FOR 1 ns;
        mem_address <= x"0000015a"; -- 346
        WAIT FOR 1 ns;
        mem_address <= x"0000015b"; -- 347
        WAIT FOR 1 ns;
        mem_address <= x"0000015c"; -- 348
        WAIT FOR 1 ns;
        mem_address <= x"0000015d"; -- 349
        WAIT FOR 1 ns;
        mem_address <= x"0000015e"; -- 350
        WAIT FOR 1 ns;
        mem_address <= x"0000015f"; -- 351
        WAIT FOR 1 ns;
        mem_address <= x"00000160"; -- 352
        WAIT FOR 1 ns;
        mem_address <= x"00000161"; -- 353
        WAIT FOR 1 ns;
        mem_address <= x"00000162"; -- 354
        WAIT FOR 1 ns;
        mem_address <= x"00000163"; -- 355
        WAIT FOR 1 ns;
        mem_address <= x"00000164"; -- 356
        WAIT FOR 1 ns;
        mem_address <= x"00000165"; -- 357
        WAIT FOR 1 ns;
        mem_address <= x"00000166"; -- 358
        WAIT FOR 1 ns;
        mem_address <= x"00000167"; -- 359
        WAIT FOR 1 ns;
        mem_address <= x"00000168"; -- 360
        WAIT FOR 1 ns;
        mem_address <= x"00000169"; -- 361
        WAIT FOR 1 ns;
        mem_address <= x"0000016a"; -- 362
        WAIT FOR 1 ns;
        mem_address <= x"0000016b"; -- 363
        WAIT FOR 1 ns;
        mem_address <= x"0000016c"; -- 364
        WAIT FOR 1 ns;
        mem_address <= x"0000016d"; -- 365
        WAIT FOR 1 ns;
        mem_address <= x"0000016e"; -- 366
        WAIT FOR 1 ns;
        mem_address <= x"0000016f"; -- 367
        WAIT FOR 1 ns;
        mem_address <= x"00000170"; -- 368
        WAIT FOR 1 ns;
        mem_address <= x"00000171"; -- 369
        WAIT FOR 1 ns;
        mem_address <= x"00000172"; -- 370
        WAIT FOR 1 ns;
        mem_address <= x"00000173"; -- 371
        WAIT FOR 1 ns;
        mem_address <= x"00000174"; -- 372
        WAIT FOR 1 ns;
        mem_address <= x"00000175"; -- 373
        WAIT FOR 1 ns;
        mem_address <= x"00000176"; -- 374
        WAIT FOR 1 ns;
        mem_address <= x"00000177"; -- 375
        WAIT FOR 1 ns;
        mem_address <= x"00000178"; -- 376
        WAIT FOR 1 ns;
        mem_address <= x"00000179"; -- 377
        WAIT FOR 1 ns;
        mem_address <= x"0000017a"; -- 378
        WAIT FOR 1 ns;
        mem_address <= x"0000017b"; -- 379
        WAIT FOR 1 ns;
        mem_address <= x"0000017c"; -- 380
        WAIT FOR 1 ns;
        mem_address <= x"0000017d"; -- 381
        WAIT FOR 1 ns;
        mem_address <= x"0000017e"; -- 382
        WAIT FOR 1 ns;
        mem_address <= x"0000017f"; -- 383
        WAIT FOR 1 ns;
        mem_address <= x"00000180"; -- 384
        WAIT FOR 1 ns;
        mem_address <= x"00000181"; -- 385
        WAIT FOR 1 ns;
        mem_address <= x"00000182"; -- 386
        WAIT FOR 1 ns;
        mem_address <= x"00000183"; -- 387
        WAIT FOR 1 ns;
        mem_address <= x"00000184"; -- 388
        WAIT FOR 1 ns;
        mem_address <= x"00000185"; -- 389
        WAIT FOR 1 ns;
        mem_address <= x"00000186"; -- 390
        WAIT FOR 1 ns;
        mem_address <= x"00000187"; -- 391
        WAIT FOR 1 ns;
        mem_address <= x"00000188"; -- 392
        WAIT FOR 1 ns;
        mem_address <= x"00000189"; -- 393
        WAIT FOR 1 ns;
        mem_address <= x"0000018a"; -- 394
        WAIT FOR 1 ns;
        mem_address <= x"0000018b"; -- 395
        WAIT FOR 1 ns;
        mem_address <= x"0000018c"; -- 396
        WAIT FOR 1 ns;
        mem_address <= x"0000018d"; -- 397
        WAIT FOR 1 ns;
        mem_address <= x"0000018e"; -- 398
        WAIT FOR 1 ns;
        mem_address <= x"0000018f"; -- 399
        WAIT FOR 1 ns;
        mem_address <= x"00000190"; -- 400
        WAIT FOR 1 ns;
        mem_address <= x"00000191"; -- 401
        WAIT FOR 1 ns;
        mem_address <= x"00000192"; -- 402
        WAIT FOR 1 ns;
        mem_address <= x"00000193"; -- 403
        WAIT FOR 1 ns;
        mem_address <= x"00000194"; -- 404
        WAIT FOR 1 ns;
        mem_address <= x"00000195"; -- 405
        WAIT FOR 1 ns;
        mem_address <= x"00000196"; -- 406
        WAIT FOR 1 ns;
        mem_address <= x"00000197"; -- 407
        WAIT FOR 1 ns;
        mem_address <= x"00000198"; -- 408
        WAIT FOR 1 ns;
        mem_address <= x"00000199"; -- 409
        WAIT FOR 1 ns;
        mem_address <= x"0000019a"; -- 410
        WAIT FOR 1 ns;
        mem_address <= x"0000019b"; -- 411
        WAIT FOR 1 ns;
        mem_address <= x"0000019c"; -- 412
        WAIT FOR 1 ns;
        mem_address <= x"0000019d"; -- 413
        WAIT FOR 1 ns;
        mem_address <= x"0000019e"; -- 414
        WAIT FOR 1 ns;
        mem_address <= x"0000019f"; -- 415
        WAIT FOR 1 ns;
        mem_address <= x"000001a0"; -- 416
        WAIT FOR 1 ns;
        mem_address <= x"000001a1"; -- 417
        WAIT FOR 1 ns;
        mem_address <= x"000001a2"; -- 418
        WAIT FOR 1 ns;
        mem_address <= x"000001a3"; -- 419
        WAIT FOR 1 ns;
        mem_address <= x"000001a4"; -- 420
        WAIT FOR 1 ns;
        mem_address <= x"000001a5"; -- 421
        WAIT FOR 1 ns;
        mem_address <= x"000001a6"; -- 422
        WAIT FOR 1 ns;
        mem_address <= x"000001a7"; -- 423
        WAIT FOR 1 ns;
        mem_address <= x"000001a8"; -- 424
        WAIT FOR 1 ns;
        mem_address <= x"000001a9"; -- 425
        WAIT FOR 1 ns;
        mem_address <= x"000001aa"; -- 426
        WAIT FOR 1 ns;
        mem_address <= x"000001ab"; -- 427
        WAIT FOR 1 ns;
        mem_address <= x"000001ac"; -- 428
        WAIT FOR 1 ns;
        mem_address <= x"000001ad"; -- 429
        WAIT FOR 1 ns;
        mem_address <= x"000001ae"; -- 430
        WAIT FOR 1 ns;
        mem_address <= x"000001af"; -- 431
        WAIT FOR 1 ns;
        mem_address <= x"000001b0"; -- 432
        WAIT FOR 1 ns;
        mem_address <= x"000001b1"; -- 433
        WAIT FOR 1 ns;
        mem_address <= x"000001b2"; -- 434
        WAIT FOR 1 ns;
        mem_address <= x"000001b3"; -- 435
        WAIT FOR 1 ns;
        mem_address <= x"000001b4"; -- 436
        WAIT FOR 1 ns;
        mem_address <= x"000001b5"; -- 437
        WAIT FOR 1 ns;
        mem_address <= x"000001b6"; -- 438
        WAIT FOR 1 ns;
        mem_address <= x"000001b7"; -- 439
        WAIT FOR 1 ns;
        mem_address <= x"000001b8"; -- 440
        WAIT FOR 1 ns;
        mem_address <= x"000001b9"; -- 441
        WAIT FOR 1 ns;
        mem_address <= x"000001ba"; -- 442
        WAIT FOR 1 ns;
        mem_address <= x"000001bb"; -- 443
        WAIT FOR 1 ns;
        mem_address <= x"000001bc"; -- 444
        WAIT FOR 1 ns;
        mem_address <= x"000001bd"; -- 445
        WAIT FOR 1 ns;
        mem_address <= x"000001be"; -- 446
        WAIT FOR 1 ns;
        mem_address <= x"000001bf"; -- 447
        WAIT FOR 1 ns;
        mem_address <= x"000001c0"; -- 448
        WAIT FOR 1 ns;
        mem_address <= x"000001c1"; -- 449
        WAIT FOR 1 ns;
        mem_address <= x"000001c2"; -- 450
        WAIT FOR 1 ns;
        mem_address <= x"000001c3"; -- 451
        WAIT FOR 1 ns;
        mem_address <= x"000001c4"; -- 452
        WAIT FOR 1 ns;
        mem_address <= x"000001c5"; -- 453
        WAIT FOR 1 ns;
        mem_address <= x"000001c6"; -- 454
        WAIT FOR 1 ns;
        mem_address <= x"000001c7"; -- 455
        WAIT FOR 1 ns;
        mem_address <= x"000001c8"; -- 456
        WAIT FOR 1 ns;
        mem_address <= x"000001c9"; -- 457
        WAIT FOR 1 ns;
        mem_address <= x"000001ca"; -- 458
        WAIT FOR 1 ns;
        mem_address <= x"000001cb"; -- 459
        WAIT FOR 1 ns;
        mem_address <= x"000001cc"; -- 460
        WAIT FOR 1 ns;
        mem_address <= x"000001cd"; -- 461
        WAIT FOR 1 ns;
        mem_address <= x"000001ce"; -- 462
        WAIT FOR 1 ns;
        mem_address <= x"000001cf"; -- 463
        WAIT FOR 1 ns;
        mem_address <= x"000001d0"; -- 464
        WAIT FOR 1 ns;
        mem_address <= x"000001d1"; -- 465
        WAIT FOR 1 ns;
        mem_address <= x"000001d2"; -- 466
        WAIT FOR 1 ns;
        mem_address <= x"000001d3"; -- 467
        WAIT FOR 1 ns;
        mem_address <= x"000001d4"; -- 468
        WAIT FOR 1 ns;
        mem_address <= x"000001d5"; -- 469
        WAIT FOR 1 ns;
        mem_address <= x"000001d6"; -- 470
        WAIT FOR 1 ns;
        mem_address <= x"000001d7"; -- 471
        WAIT FOR 1 ns;
        mem_address <= x"000001d8"; -- 472
        WAIT FOR 1 ns;
        mem_address <= x"000001d9"; -- 473
        WAIT FOR 1 ns;
        mem_address <= x"000001da"; -- 474
        WAIT FOR 1 ns;
        mem_address <= x"000001db"; -- 475
        WAIT FOR 1 ns;
        mem_address <= x"000001dc"; -- 476
        WAIT FOR 1 ns;
        mem_address <= x"000001dd"; -- 477
        WAIT FOR 1 ns;
        mem_address <= x"000001de"; -- 478
        WAIT FOR 1 ns;
        mem_address <= x"000001df"; -- 479
        WAIT FOR 1 ns;
        mem_address <= x"000001e0"; -- 480
        WAIT FOR 1 ns;
        mem_address <= x"000001e1"; -- 481
        WAIT FOR 1 ns;
        mem_address <= x"000001e2"; -- 482
        WAIT FOR 1 ns;
        mem_address <= x"000001e3"; -- 483
        WAIT FOR 1 ns;
        mem_address <= x"000001e4"; -- 484
        WAIT FOR 1 ns;
        mem_address <= x"000001e5"; -- 485
        WAIT FOR 1 ns;
        mem_address <= x"000001e6"; -- 486
        WAIT FOR 1 ns;
        mem_address <= x"000001e7"; -- 487
        WAIT FOR 1 ns;
        mem_address <= x"000001e8"; -- 488
        WAIT FOR 1 ns;
        mem_address <= x"000001e9"; -- 489
        WAIT FOR 1 ns;
        mem_address <= x"000001ea"; -- 490
        WAIT FOR 1 ns;
        mem_address <= x"000001eb"; -- 491
        WAIT FOR 1 ns;
        mem_address <= x"000001ec"; -- 492
        WAIT FOR 1 ns;
        mem_address <= x"000001ed"; -- 493
        WAIT FOR 1 ns;
        mem_address <= x"000001ee"; -- 494
        WAIT FOR 1 ns;
        mem_address <= x"000001ef"; -- 495
        WAIT FOR 1 ns;
        mem_address <= x"000001f0"; -- 496
        WAIT FOR 1 ns;
        mem_address <= x"000001f1"; -- 497
        WAIT FOR 1 ns;
        mem_address <= x"000001f2"; -- 498
        WAIT FOR 1 ns;
        mem_address <= x"000001f3"; -- 499
        WAIT FOR 1 ns;
        mem_address <= x"000001f4"; -- 500
        WAIT FOR 1 ns;
        mem_address <= x"000001f5"; -- 501
        WAIT FOR 1 ns;
        mem_address <= x"000001f6"; -- 502
        WAIT FOR 1 ns;
        mem_address <= x"000001f7"; -- 503
        WAIT FOR 1 ns;
        mem_address <= x"000001f8"; -- 504
        WAIT FOR 1 ns;
        mem_address <= x"000001f9"; -- 505
        WAIT FOR 1 ns;
        mem_address <= x"000001fa"; -- 506
        WAIT FOR 1 ns;
        mem_address <= x"000001fb"; -- 507
        WAIT FOR 1 ns;
        mem_address <= x"000001fc"; -- 508
        WAIT FOR 1 ns;
        mem_address <= x"000001fd"; -- 509
        WAIT FOR 1 ns;
        mem_address <= x"000001fe"; -- 510
        WAIT FOR 1 ns;
        mem_address <= x"000001ff"; -- 511

        WAIT FOR 2 ns;

        -- attempting to write with write_enable turned off
        mem_address <= x"00000000"; -- 0
        data_in <= x"000000b5";
        WAIT FOR 1 ns;
        mem_address <= x"00000001"; -- 1
        data_in <= x"000002b3";
        WAIT FOR 1 ns;
        mem_address <= x"00000002"; -- 2
        data_in <= x"ffff1123";
        WAIT FOR 1 ns;

        -- overwriting existing memory
        write_enable <= '1';

        mem_address <= x"00000000"; -- 0
        data_in <= x"ffffffff";
        WAIT FOR 1 ns;
        mem_address <= x"00000001"; -- 1
        data_in <= x"00000004";
        WAIT FOR 1 ns;
        mem_address <= x"00000002"; -- 2
        data_in <= x"00000008";
        WAIT FOR 1 ns;
        mem_address <= x"00000003"; -- 3
        data_in <= x"0000000c";
        WAIT FOR 1 ns;
        mem_address <= x"00000004"; -- 4
        data_in <= x"00000010";
        WAIT FOR 1 ns;
        mem_address <= x"00000005"; -- 5
        data_in <= x"00000014";
        WAIT FOR 1 ns;
        mem_address <= x"00000006"; -- 6
        data_in <= x"00000018";
        WAIT FOR 1 ns;
        mem_address <= x"00000007"; -- 7
        data_in <= x"0000001c";
        WAIT FOR 1 ns;
        mem_address <= x"00000008"; -- 8
        data_in <= x"00000020";
        WAIT FOR 1 ns;
        mem_address <= x"00000009"; -- 9
        data_in <= x"00000024";
        WAIT FOR 1 ns;
        mem_address <= x"0000000a"; -- 10
        data_in <= x"00000028";
        WAIT FOR 1 ns;
        mem_address <= x"0000000b"; -- 11
        data_in <= x"0000002c";
        WAIT FOR 1 ns;
        mem_address <= x"0000000c"; -- 12
        data_in <= x"00000030";
        WAIT FOR 1 ns;
        mem_address <= x"0000000d"; -- 13
        data_in <= x"00000034";
        WAIT FOR 1 ns;
        mem_address <= x"0000000e"; -- 14
        data_in <= x"00000038";
        WAIT FOR 1 ns;
        mem_address <= x"0000000f"; -- 15
        data_in <= x"0000003c";
        WAIT FOR 1 ns;
        mem_address <= x"00000010"; -- 16
        data_in <= x"00000040";
        WAIT FOR 1 ns;
        mem_address <= x"00000011"; -- 17
        data_in <= x"00000044";
        WAIT FOR 1 ns;
        mem_address <= x"00000012"; -- 18
        data_in <= x"00000048";
        WAIT FOR 1 ns;
        mem_address <= x"00000013"; -- 19
        data_in <= x"0000004c";
        WAIT FOR 1 ns;
        mem_address <= x"00000014"; -- 20
        data_in <= x"00000050";
        WAIT FOR 1 ns;
        mem_address <= x"00000015"; -- 21
        data_in <= x"00000054";
        WAIT FOR 1 ns;
        mem_address <= x"00000016"; -- 22
        data_in <= x"00000058";
        WAIT FOR 1 ns;
        mem_address <= x"00000017"; -- 23
        data_in <= x"0000005c";
        WAIT FOR 1 ns;
        mem_address <= x"00000018"; -- 24
        data_in <= x"00000060";
        WAIT FOR 1 ns;
        mem_address <= x"00000019"; -- 25
        data_in <= x"00000064";
        WAIT FOR 1 ns;
        mem_address <= x"0000001a"; -- 26
        data_in <= x"00000068";
        WAIT FOR 1 ns;
        mem_address <= x"0000001b"; -- 27
        data_in <= x"0000006c";
        WAIT FOR 1 ns;
        mem_address <= x"0000001c"; -- 28
        data_in <= x"00000070";
        WAIT FOR 1 ns;
        mem_address <= x"0000001d"; -- 29
        data_in <= x"00000074";
        WAIT FOR 1 ns;
        mem_address <= x"0000001e"; -- 30
        data_in <= x"00000078";
        WAIT FOR 1 ns;
        mem_address <= x"0000001f"; -- 31
        data_in <= x"0000007c";

        WAIT;
    END PROCESS;
END;