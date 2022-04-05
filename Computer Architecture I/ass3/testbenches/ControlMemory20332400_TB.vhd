LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ctrl_mem_tb IS
END ctrl_mem_tb;

ARCHITECTURE Behavioural OF ctrl_mem_tb is
    COMPONENT ctrl_mem PORT (
        IN_CAR : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        FL : OUT STD_LOGIC;
        RZ : OUT STD_LOGIC;
        RN : OUT STD_LOGIC;
        RC : OUT STD_LOGIC;
        RV : OUT STD_LOGIC;
        MW : OUT STD_LOGIC;
        MM : OUT STD_LOGIC;
        RW : OUT STD_LOGIC;
        MD : OUT STD_LOGIC;
        FS : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        MB : OUT STD_LOGIC;
        TB : OUT STD_LOGIC;
        TA : OUT STD_LOGIC;
        TD : OUT STD_LOGIC;
        PL : OUT STD_LOGIC;
        PI : OUT STD_LOGIC;
        IL : OUT STD_LOGIC;
        MC : OUT STD_LOGIC;
        MS : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
        NA : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
        );
    END COMPONENT;
    -- input
    SIGNAL IN_CAR : STD_LOGIC_VECTOR(16 DOWNTO 0);

    --output
    SIGNAL FL, RZ, RN, RC, RV, MW, MM, RW, MD, MB, TB, TA, TD, PL, PI, IL, MC : STD_LOGIC;
    SIGNAL MS : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL FS : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL NA : STD_LOGIC_VECTOR(16 DOWNTO 0);
BEGIN

    uut : ctrl_mem PORT MAP(
        IN_CAR => IN_CAR,
        FL => FL,
        RZ => RZ,
        RN => RN,
        RC => RC,
        RV => RV,
        MW => MW,
        MM => MM,
        RW => RW,
        MD => MD,
        FS => FS,
        MB => MB,
        TB => TB,
        TA => TA,
        TD => TD,
        PL => PL,
        PI => PI,
        IL => IL,
        MC => MC,
        MS => MS,
        NA => NA
    );

    stim_proc : PROCESS
    BEGIN
        IN_CAR <= "00000000000000000"; -- 0
        wait for 2 ns;
        IN_CAR <= "00000000000000001"; -- 1
        wait for 2 ns;
        IN_CAR <= "00000000000000010"; -- 2
        wait for 2 ns;
        IN_CAR <= "00000000000000011"; -- 3
        wait for 2 ns;
        IN_CAR <= "00000000000000100"; -- 4
        wait for 2 ns;
        IN_CAR <= "00000000000000101"; -- 5
        wait for 2 ns;
        IN_CAR <= "00000000000000110"; -- 6
        wait for 2 ns;
        IN_CAR <= "00000000000000111"; -- 7
        wait for 2 ns;
        IN_CAR <= "00000000000001000"; -- 8
        wait for 2 ns;
        IN_CAR <= "00000000000001001"; -- 9
        wait for 2 ns;
        IN_CAR <= "00000000000001010"; -- a
        wait for 2 ns;
        IN_CAR <= "00000000000001011"; -- b
        wait for 2 ns;
        IN_CAR <= "00000000000001100"; -- c
        wait for 2 ns;
        IN_CAR <= "00000000000001101"; -- d
        wait for 2 ns;
        IN_CAR <= "00000000000001110"; -- e
        wait for 2 ns;
        IN_CAR <= "00000000000001111"; -- f
        wait for 2 ns;
        IN_CAR <= "00000000000010000"; -- 10
        wait for 2 ns;
        IN_CAR <= "00000000000010001"; -- 11
        wait for 2 ns;
        IN_CAR <= "00000000000010010"; -- 12
        wait for 2 ns;
        IN_CAR <= "00000000000010011"; -- 13
        wait for 2 ns;
        IN_CAR <= "00000000000010100"; -- 14
        wait for 2 ns;
        IN_CAR <= "00000000000010101"; -- 15
        wait for 2 ns;
        IN_CAR <= "00000000000010110"; -- 16
        wait for 2 ns;
        IN_CAR <= "00000000000010111"; -- 17
        wait for 2 ns;
        IN_CAR <= "00000000000011000"; -- 18
        wait for 2 ns;
        IN_CAR <= "00000000000011001"; -- 19
        wait for 2 ns;
        IN_CAR <= "00000000000011010"; -- 1a
        wait for 2 ns;
        IN_CAR <= "00000000000011011"; -- 1b
        wait for 2 ns;
        IN_CAR <= "00000000000011100"; -- 1c
        wait for 2 ns;
        IN_CAR <= "00000000000011101"; -- 1d
        wait for 2 ns;
        IN_CAR <= "00000000000011110"; -- 1e
        wait for 2 ns;
        IN_CAR <= "00000000000011111"; -- 1f
        wait for 2 ns;
        IN_CAR <= "00000000000100000"; -- 20
        wait for 2 ns;
        IN_CAR <= "00000000000100001"; -- 21
        wait for 2 ns;
        IN_CAR <= "00000000000100010"; -- 22
        wait for 2 ns;
        IN_CAR <= "00000000000100011"; -- 23
        wait for 2 ns;
        IN_CAR <= "00000000000100100"; -- 24
        wait for 2 ns;
        IN_CAR <= "00000000000100101"; -- 25
        wait for 2 ns;
        IN_CAR <= "00000000000100110"; -- 26
        wait for 2 ns;
        IN_CAR <= "00000000000100111"; -- 27
        wait for 2 ns;
        IN_CAR <= "00000000000101000"; -- 28
        wait for 2 ns;
        IN_CAR <= "00000000000101001"; -- 29
        wait for 2 ns;
        IN_CAR <= "00000000000101010"; -- 2a
        wait for 2 ns;
        IN_CAR <= "00000000000101011"; -- 2b
        wait for 2 ns;
        IN_CAR <= "00000000000101100"; -- 2c
        wait for 2 ns;
        IN_CAR <= "00000000000101101"; -- 2d
        wait for 2 ns;
        IN_CAR <= "00000000000101110"; -- 2e
        wait for 2 ns;
        IN_CAR <= "00000000000101111"; -- 2f
        wait for 2 ns;
        IN_CAR <= "00000000000110000"; -- 30
        wait for 2 ns;
        IN_CAR <= "00000000000110001"; -- 31
        wait for 2 ns;
        IN_CAR <= "00000000000110010"; -- 32
        wait for 2 ns;
        IN_CAR <= "00000000000110011"; -- 33
        wait for 2 ns;
        IN_CAR <= "00000000000110100"; -- 34
        wait for 2 ns;
        IN_CAR <= "00000000000110101"; -- 35
        wait for 2 ns;
        IN_CAR <= "00000000000110110"; -- 36
        wait for 2 ns;
        IN_CAR <= "00000000000110111"; -- 37
        wait for 2 ns;
        IN_CAR <= "00000000000111000"; -- 38
        wait for 2 ns;
        IN_CAR <= "00000000000111001"; -- 39
        wait for 2 ns;
        IN_CAR <= "00000000000111010"; -- 3a
        wait for 2 ns;
        IN_CAR <= "00000000000111011"; -- 3b
        wait for 2 ns;
        IN_CAR <= "00000000000111100"; -- 3c
        wait for 2 ns;
        IN_CAR <= "00000000000111101"; -- 3d
        wait for 2 ns;
        IN_CAR <= "00000000000111110"; -- 3e
        wait for 2 ns;
        IN_CAR <= "00000000000111111"; -- 3f
        wait for 2 ns;
        IN_CAR <= "00000000001000000"; -- 40
        wait for 2 ns;
        IN_CAR <= "00000000001000001"; -- 41
        wait for 2 ns;
        IN_CAR <= "00000000001000010"; -- 42
        wait for 2 ns;
        IN_CAR <= "00000000001000011"; -- 43
        wait for 2 ns;
        IN_CAR <= "00000000001000100"; -- 44
        wait for 2 ns;
        IN_CAR <= "00000000001000101"; -- 45
        wait for 2 ns;
        IN_CAR <= "00000000001000110"; -- 46
        wait for 2 ns;
        IN_CAR <= "00000000001000111"; -- 47
        wait for 2 ns;
        IN_CAR <= "00000000001001000"; -- 48
        wait for 2 ns;
        IN_CAR <= "00000000001001001"; -- 49
        wait for 2 ns;
        IN_CAR <= "00000000001001010"; -- 4a
        wait for 2 ns;
        IN_CAR <= "00000000001001011"; -- 4b
        wait for 2 ns;
        IN_CAR <= "00000000001001100"; -- 4c
        wait for 2 ns;
        IN_CAR <= "00000000001001101"; -- 4d
        wait for 2 ns;
        IN_CAR <= "00000000001001110"; -- 4e
        wait for 2 ns;
        IN_CAR <= "00000000001001111"; -- 4f
        wait for 2 ns;
        IN_CAR <= "00000000001010000"; -- 50
        wait for 2 ns;
        IN_CAR <= "00000000001010001"; -- 51
        wait for 2 ns;
        IN_CAR <= "00000000001010010"; -- 52
        wait for 2 ns;
        IN_CAR <= "00000000001010011"; -- 53
        wait for 2 ns;
        IN_CAR <= "00000000001010100"; -- 54
        wait for 2 ns;
        IN_CAR <= "00000000001010101"; -- 55
        wait for 2 ns;
        IN_CAR <= "00000000001010110"; -- 56
        wait for 2 ns;
        IN_CAR <= "00000000001010111"; -- 57
        wait for 2 ns;
        IN_CAR <= "00000000001011000"; -- 58
        wait for 2 ns;
        IN_CAR <= "00000000001011001"; -- 59
        wait for 2 ns;
        IN_CAR <= "00000000001011010"; -- 5a
        wait for 2 ns;
        IN_CAR <= "00000000001011011"; -- 5b
        wait for 2 ns;
        IN_CAR <= "00000000001011100"; -- 5c
        wait for 2 ns;
        IN_CAR <= "00000000001011101"; -- 5d
        wait for 2 ns;
        IN_CAR <= "00000000001011110"; -- 5e
        wait for 2 ns;
        IN_CAR <= "00000000001011111"; -- 5f
        wait for 2 ns;
        IN_CAR <= "00000000001100000"; -- 60
        wait for 2 ns;
        IN_CAR <= "00000000001100001"; -- 61
        wait for 2 ns;
        IN_CAR <= "00000000001100010"; -- 62
        wait for 2 ns;
        IN_CAR <= "00000000001100011"; -- 63
        wait for 2 ns;
        IN_CAR <= "00000000001100100"; -- 64
        wait for 2 ns;
        IN_CAR <= "00000000001100101"; -- 65
        wait for 2 ns;
        IN_CAR <= "00000000001100110"; -- 66
        wait for 2 ns;
        IN_CAR <= "00000000001100111"; -- 67
        wait for 2 ns;
        IN_CAR <= "00000000001101000"; -- 68
        wait for 2 ns;
        IN_CAR <= "00000000001101001"; -- 69
        wait for 2 ns;
        IN_CAR <= "00000000001101010"; -- 6a
        wait for 2 ns;
        IN_CAR <= "00000000001101011"; -- 6b
        wait for 2 ns;
        IN_CAR <= "00000000001101100"; -- 6c
        wait for 2 ns;
        IN_CAR <= "00000000001101101"; -- 6d
        wait for 2 ns;
        IN_CAR <= "00000000001101110"; -- 6e
        wait for 2 ns;
        IN_CAR <= "00000000001101111"; -- 6f
        wait for 2 ns;
        IN_CAR <= "00000000001110000"; -- 70
        wait for 2 ns;
        IN_CAR <= "00000000001110001"; -- 71
        wait for 2 ns;
        IN_CAR <= "00000000001110010"; -- 72
        wait for 2 ns;
        IN_CAR <= "00000000001110011"; -- 73
        wait for 2 ns;
        IN_CAR <= "00000000001110100"; -- 74
        wait for 2 ns;
        IN_CAR <= "00000000001110101"; -- 75
        wait for 2 ns;
        IN_CAR <= "00000000001110110"; -- 76
        wait for 2 ns;
        IN_CAR <= "00000000001110111"; -- 77
        wait for 2 ns;
        IN_CAR <= "00000000001111000"; -- 78
        wait for 2 ns;
        IN_CAR <= "00000000001111001"; -- 79
        wait for 2 ns;
        IN_CAR <= "00000000001111010"; -- 7a
        wait for 2 ns;
        IN_CAR <= "00000000001111011"; -- 7b
        wait for 2 ns;
        IN_CAR <= "00000000001111100"; -- 7c
        wait for 2 ns;
        IN_CAR <= "00000000001111101"; -- 7d
        wait for 2 ns;
        IN_CAR <= "00000000001111110"; -- 7e
        wait for 2 ns;
        IN_CAR <= "00000000001111111"; -- 7f
        wait for 2 ns;
        IN_CAR <= "00000000010000000"; -- 80
        wait for 2 ns;
        IN_CAR <= "00000000010000001"; -- 81
        wait for 2 ns;
        IN_CAR <= "00000000010000010"; -- 82
        wait for 2 ns;
        IN_CAR <= "00000000010000011"; -- 83
        wait for 2 ns;
        IN_CAR <= "00000000010000100"; -- 84
        wait for 2 ns;
        IN_CAR <= "00000000010000101"; -- 85
        wait for 2 ns;
        IN_CAR <= "00000000010000110"; -- 86
        wait for 2 ns;
        IN_CAR <= "00000000010000111"; -- 87
        wait for 2 ns;
        IN_CAR <= "00000000010001000"; -- 88
        wait for 2 ns;
        IN_CAR <= "00000000010001001"; -- 89
        wait for 2 ns;
        IN_CAR <= "00000000010001010"; -- 8a
        wait for 2 ns;
        IN_CAR <= "00000000010001011"; -- 8b
        wait for 2 ns;
        IN_CAR <= "00000000010001100"; -- 8c
        wait for 2 ns;
        IN_CAR <= "00000000010001101"; -- 8d
        wait for 2 ns;
        IN_CAR <= "00000000010001110"; -- 8e
        wait for 2 ns;
        IN_CAR <= "00000000010001111"; -- 8f
        wait for 2 ns;
        IN_CAR <= "00000000010010000"; -- 90
        wait for 2 ns;
        IN_CAR <= "00000000010010001"; -- 91
        wait for 2 ns;
        IN_CAR <= "00000000010010010"; -- 92
        wait for 2 ns;
        IN_CAR <= "00000000010010011"; -- 93
        wait for 2 ns;
        IN_CAR <= "00000000010010100"; -- 94
        wait for 2 ns;
        IN_CAR <= "00000000010010101"; -- 95
        wait for 2 ns;
        IN_CAR <= "00000000010010110"; -- 96
        wait for 2 ns;
        IN_CAR <= "00000000010010111"; -- 97
        wait for 2 ns;
        IN_CAR <= "00000000010011000"; -- 98
        wait for 2 ns;
        IN_CAR <= "00000000010011001"; -- 99
        wait for 2 ns;
        IN_CAR <= "00000000010011010"; -- 9a
        wait for 2 ns;
        IN_CAR <= "00000000010011011"; -- 9b
        wait for 2 ns;
        IN_CAR <= "00000000010011100"; -- 9c
        wait for 2 ns;
        IN_CAR <= "00000000010011101"; -- 9d
        wait for 2 ns;
        IN_CAR <= "00000000010011110"; -- 9e
        wait for 2 ns;
        IN_CAR <= "00000000010011111"; -- 9f
        wait for 2 ns;
        IN_CAR <= "00000000010100000"; -- a0
        wait for 2 ns;
        IN_CAR <= "00000000010100001"; -- a1
        wait for 2 ns;
        IN_CAR <= "00000000010100010"; -- a2
        wait for 2 ns;
        IN_CAR <= "00000000010100011"; -- a3
        wait for 2 ns;
        IN_CAR <= "00000000010100100"; -- a4
        wait for 2 ns;
        IN_CAR <= "00000000010100101"; -- a5
        wait for 2 ns;
        IN_CAR <= "00000000010100110"; -- a6
        wait for 2 ns;
        IN_CAR <= "00000000010100111"; -- a7
        wait for 2 ns;
        IN_CAR <= "00000000010101000"; -- a8
        wait for 2 ns;
        IN_CAR <= "00000000010101001"; -- a9
        wait for 2 ns;
        IN_CAR <= "00000000010101010"; -- aa
        wait for 2 ns;
        IN_CAR <= "00000000010101011"; -- ab
        wait for 2 ns;
        IN_CAR <= "00000000010101100"; -- ac
        wait for 2 ns;
        IN_CAR <= "00000000010101101"; -- ad
        wait for 2 ns;
        IN_CAR <= "00000000010101110"; -- ae
        wait for 2 ns;
        IN_CAR <= "00000000010101111"; -- af
        wait for 2 ns;
        IN_CAR <= "00000000010110000"; -- b0
        wait for 2 ns;
        IN_CAR <= "00000000010110001"; -- b1
        wait for 2 ns;
        IN_CAR <= "00000000010110010"; -- b2
        wait for 2 ns;
        IN_CAR <= "00000000010110011"; -- b3
        wait for 2 ns;
        IN_CAR <= "00000000010110100"; -- b4
        wait for 2 ns;
        IN_CAR <= "00000000010110101"; -- b5
        wait for 2 ns;
        IN_CAR <= "00000000010110110"; -- b6
        wait for 2 ns;
        IN_CAR <= "00000000010110111"; -- b7
        wait for 2 ns;
        IN_CAR <= "00000000010111000"; -- b8
        wait for 2 ns;
        IN_CAR <= "00000000010111001"; -- b9
        wait for 2 ns;
        IN_CAR <= "00000000010111010"; -- ba
        wait for 2 ns;
        IN_CAR <= "00000000010111011"; -- bb
        wait for 2 ns;
        IN_CAR <= "00000000010111100"; -- bc
        wait for 2 ns;
        IN_CAR <= "00000000010111101"; -- bd
        wait for 2 ns;
        IN_CAR <= "00000000010111110"; -- be
        wait for 2 ns;
        IN_CAR <= "00000000010111111"; -- bf
        wait for 2 ns;
        IN_CAR <= "00000000011000000"; -- c0
        wait for 2 ns;
        IN_CAR <= "00000000011000001"; -- c1
        wait for 2 ns;
        IN_CAR <= "00000000011000010"; -- c2
        wait for 2 ns;
        IN_CAR <= "00000000011000011"; -- c3
        wait for 2 ns;
        IN_CAR <= "00000000011000100"; -- c4
        wait for 2 ns;
        IN_CAR <= "00000000011000101"; -- c5
        wait for 2 ns;
        IN_CAR <= "00000000011000110"; -- c6
        wait for 2 ns;
        IN_CAR <= "00000000011000111"; -- c7
        wait for 2 ns;
        IN_CAR <= "00000000011001000"; -- c8
        wait for 2 ns;
        IN_CAR <= "00000000011001001"; -- c9
        wait for 2 ns;
        IN_CAR <= "00000000011001010"; -- ca
        wait for 2 ns;
        IN_CAR <= "00000000011001011"; -- cb
        wait for 2 ns;
        IN_CAR <= "00000000011001100"; -- cc
        wait for 2 ns;
        IN_CAR <= "00000000011001101"; -- cd
        wait for 2 ns;
        IN_CAR <= "00000000011001110"; -- ce
        wait for 2 ns;
        IN_CAR <= "00000000011001111"; -- cf
        wait for 2 ns;
        IN_CAR <= "00000000011010000"; -- d0
        wait for 2 ns;
        IN_CAR <= "00000000011010001"; -- d1
        wait for 2 ns;
        IN_CAR <= "00000000011010010"; -- d2
        wait for 2 ns;
        IN_CAR <= "00000000011010011"; -- d3
        wait for 2 ns;
        IN_CAR <= "00000000011010100"; -- d4
        wait for 2 ns;
        IN_CAR <= "00000000011010101"; -- d5
        wait for 2 ns;
        IN_CAR <= "00000000011010110"; -- d6
        wait for 2 ns;
        IN_CAR <= "00000000011010111"; -- d7
        wait for 2 ns;
        IN_CAR <= "00000000011011000"; -- d8
        wait for 2 ns;
        IN_CAR <= "00000000011011001"; -- d9
        wait for 2 ns;
        IN_CAR <= "00000000011011010"; -- da
        wait for 2 ns;
        IN_CAR <= "00000000011011011"; -- db
        wait for 2 ns;
        IN_CAR <= "00000000011011100"; -- dc
        wait for 2 ns;
        IN_CAR <= "00000000011011101"; -- dd
        wait for 2 ns;
        IN_CAR <= "00000000011011110"; -- de
        wait for 2 ns;
        IN_CAR <= "00000000011011111"; -- df
        wait for 2 ns;
        IN_CAR <= "00000000011100000"; -- e0
        wait for 2 ns;
        IN_CAR <= "00000000011100001"; -- e1
        wait for 2 ns;
        IN_CAR <= "00000000011100010"; -- e2
        wait for 2 ns;
        IN_CAR <= "00000000011100011"; -- e3
        wait for 2 ns;
        IN_CAR <= "00000000011100100"; -- e4
        wait for 2 ns;
        IN_CAR <= "00000000011100101"; -- e5
        wait for 2 ns;
        IN_CAR <= "00000000011100110"; -- e6
        wait for 2 ns;
        IN_CAR <= "00000000011100111"; -- e7
        wait for 2 ns;
        IN_CAR <= "00000000011101000"; -- e8
        wait for 2 ns;
        IN_CAR <= "00000000011101001"; -- e9
        wait for 2 ns;
        IN_CAR <= "00000000011101010"; -- ea
        wait for 2 ns;
        IN_CAR <= "00000000011101011"; -- eb
        wait for 2 ns;
        IN_CAR <= "00000000011101100"; -- ec
        wait for 2 ns;
        IN_CAR <= "00000000011101101"; -- ed
        wait for 2 ns;
        IN_CAR <= "00000000011101110"; -- ee
        wait for 2 ns;
        IN_CAR <= "00000000011101111"; -- ef
        wait for 2 ns;
        IN_CAR <= "00000000011110000"; -- f0
        wait for 2 ns;
        IN_CAR <= "00000000011110001"; -- f1
        wait for 2 ns;
        IN_CAR <= "00000000011110010"; -- f2
        wait for 2 ns;
        IN_CAR <= "00000000011110011"; -- f3
        wait for 2 ns;
        IN_CAR <= "00000000011110100"; -- f4
        wait for 2 ns;
        IN_CAR <= "00000000011110101"; -- f5
        wait for 2 ns;
        IN_CAR <= "00000000011110110"; -- f6
        wait for 2 ns;
        IN_CAR <= "00000000011110111"; -- f7
        wait for 2 ns;
        IN_CAR <= "00000000011111000"; -- f8
        wait for 2 ns;
        IN_CAR <= "00000000011111001"; -- f9
        wait for 2 ns;
        IN_CAR <= "00000000011111010"; -- fa
        wait for 2 ns;
        IN_CAR <= "00000000011111011"; -- fb
        wait for 2 ns;
        IN_CAR <= "00000000011111100"; -- fc
        wait for 2 ns;
        IN_CAR <= "00000000011111101"; -- fd
        wait for 2 ns;
        IN_CAR <= "00000000011111110"; -- fe
        wait for 2 ns;
        IN_CAR <= "00000000011111111"; -- ff
        WAIT;
    END PROCESS;

END Behavioural;