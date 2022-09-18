#
# string area
#
HELLO		.BYTE	LF, "EON ROM 0.2.0", 0
MENU		.BYTE	LF
		.BYTE	"1. GET/SET DATE   5. DISK INFO", LF
		.BYTE	"2. MEMORY MONITOR", LF
		.BYTE	"3. FORTH          9. ILLEGAL", LF
		.BYTE	"4. BENCHMARK      0. RESET", LF
		.BYTE	"OPTION? ", 0
FORTH_HELLO	.BYTE	LF, "JONES FORTH READY", LF, 0
PARSER_ERROR	.BYTE	"PARSER ERROR", LF, 0
DATE_HEADER	.BYTE	CR, "  ", 0
DATE_FOOTER	.BYTE	" (TAB/SPACE=NEXT, ESC/X=EXIT ENTER=UPDATE)", ESC, "[", 0
DATE_COLS	.BYTE	3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 19
DATE_WRITTEN	.BYTE	LF, "DATE WRITTEN !", 0
HTAB		.BYTE	"0123456789ABCDEF"
ESCOFF		.BYTE	"[K", 0
MMON_HEADER	.BYTE	LF, " (ESC/X=EXIT ENTER=GOTO TAB=NEXT Z=ZERO I=INC LINES P/K=POKE R=RUN)", LF, 0
EXC_HEADER	.BYTE	LF, "EXCEPTION ", 0
EXC_MIDDLE	.BYTE	" AT ", 0
EXC_FOOTER	.BYTE	" WITH REGS:", 0
BENCH_HEADER	.BYTE	LF, "CRC32 FIRST 64 ROM BYTES 100 TIMES: ", 0
BENCH_FOOTER	.BYTE	" TENTHS OF A SECOND ", 0
DISK_HEADER	.BYTE	LF, "DISK DETECTION ... ", 0
DISK_FOOTER	.BYTE	" SECTORS", LF, 0
DISK_TIMEOUT	.BYTE	"TIMEOUT !", LF, 0
