/*** includes ***/

// Headers being included use the macros to decide which features to expose
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define LYTE_VERSION "1.0.0"
#define CTRL_KEY(k) ((k) & 0x1f) // Bitmask in hexadecimal to mirror a Ctrl key combination
#define TAB_STOP 8
#define QUIT_TIMES 3

enum editorKey {
  BACKSPACE = 127, // Prevents insertion into text
  ARROW_UP = 1000, // Other constants get incremented
  ARROW_LEFT,
  ARROW_DOWN,
  ARROW_RIGHT,
  PAGE_UP,
  PAGE_DOWN,
  HOME_KEY,
  END_KEY,
  DEL_KEY
};

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ****/

struct editorSyntax {
  char *fileType;
  char **fileMatch; // Array of strings, where each string contains a pattern to match a filename against
  char **keywords; // Null terminated array of strings, each containing a keyword
  char *singleLineCommentStart;
  char *multiLineCommentStart;
  char *multiLineCommentEnd;
  int flags; // Bit field that will contain flags for whether to highlight numbers and strings for the filetype
};

typedef struct editorRow { // Stores line of text as a pointer to the dynamically allocated character data and length
  int idx;
  int size;
  int rSize;
  char *chars;
  char *render;
  unsigned char *hl; // Highlight -> array of unsigned char values corresponding to a character in render
  int hlOpenComment; // If previous line is part of an unclosed multi-line comment
} editorRow;

struct editorConfig { // Global struct to contain our editor state
  int cx, cy; // Cursor position within the file, not on screen (index into cursor field of an editorRow)
  int rx; // Horizontal coordinate variable (index into render field of an editorRow)
  int rowOff; // Row of file the user is currently scrolled to
  int colOff; // Column of file the user is currently scrolled to
  int screenRows;
  int screenCols;
  int numRows;
  editorRow *rows; // Array of editorRow structs
  int dirty; // Flag to indicate if a file has been modified since opening or saving the file
  char *fileName;
  char statusMsg[80];
  struct editorSyntax *syntax;
  time_t statusMsgTime;
  struct termios origTermios;
};

struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", ".hpp", ".cc", NULL};
char *C_HL_keywords[] = {
  // C keywords
  "break", "case", "continue", "default", "do", "else", "enum", "extern",
  "for", "goto", "if", "register", "return", "sizeof", "static", "struct",
  "switch", "typedef", "union", "volatile", "while", "NULL",
  // C++ keywords
  "alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor", "catch",
  "class", "compl", "concept", "const_cast", "consteval", "constexpr", "constinit",
  "co_await", "co_return", "co_yield", "decltype", "delete", "dynamic_cast",
  "explicit", "export", "false", "friend", "inline", "mutable", "namespace",
  "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
  "private", "protected", "public", "reinterpret_cast", "requires", "static_assert",
  "static_cast", "template", "this", "thread_local", "throw", "true", "try",
  "typeid", "typename", "using", "virtual", "xor", "xor_eq",
  // Differentiating the two types of keywords by terminating with a pipe
  "auto|", "bool|", "char|", "char16_t|", "char32_t|", "char8_t|",
  "const|", "double|", "final|", "float|", "int|", "long|",
  "short|", "signed|", "unsigned|", "void|", NULL
};

char *Java_HL_extensions[] = {".java", NULL};
char *Java_HL_keywords[] = {
  "abstract", "assert", "break", "case", "catch", "class", "continue",
  "default", "do", "else", "enum", "exports", "extends", "finally", "for",
  "goto", "if", "implements", "import", "instanceof", "interface", "module",
  "native", "new", "package", "private", "protected", "public", "requires",
  "return", "strictfp", "super", "switch", "this", "throw", "throws",
  "transient", "try", "volatile", "while",

  "boolean|", "byte|", "char|", "const|", "double|", "final|", "float|",
  "int|", "long|", "short|", "static|", "synchronized|", "var|", "void|", NULL
};

struct editorSyntax HLDB[] = { // Highlight database
  {
    "c",
    C_HL_extensions, // fileMatch
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS // Flags
  },
  {
    "java",
    Java_HL_extensions,
    Java_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  }
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0])) // Stores length of HLDB array

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  perror(s);
  exit(1);
}

void disableRawMode() {
  if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.origTermios) == -1) { // Sets terminal attributes back to their original state
    die("tcsetattr");
  }
}

void enableRawMode() {
  if(tcgetattr(STDIN_FILENO, &E.origTermios) == -1) {
    die("tcgetattr");
  }
  atexit(disableRawMode);
  struct termios raw = E.origTermios;
  // Reads byte-by-byte by turning off canonical mode through ICANON flag
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN); // Turns off echo feature through bitwise NOT, and bitwise AND with the flags field
  raw.c_iflag &= ~(IXON | ICRNL | BRKINT |INPCK | ISTRIP); // Disables Ctrl-S and Ctrl-Q and carriage returns into newlines
  raw.c_oflag &= ~(OPOST); // Turns off output processing
  raw.c_cflag |= (CS8); // Sets character size to 8 bits per byte
  raw.c_cc[VMIN] = 0; // Minimum number of bytes of input needed before read() can return is 0 so it returns on input
  raw.c_cc[VTIME] = 1; // Maximum amount of time to wait before read() returns (0.1 seconds)
  if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) { // TCSAFLUSH argument waits for all output to be written to terminal
    die("tcsettatr");
  }
}

int editorReadKey() { // Waits for one keypress and return it
  int nread; // Number of bytes read by read()
  char c; // Holds single character input
  while((nread = read(STDIN_FILENO, &c, 1)) != 1) { // Returns the number of bytes read until exactly 1 is read
    if(nread == -1 && errno != EAGAIN) {
      die("read");
    }
  }
  if(c == '\x1b') {
    char seq[3];
    if(read(STDIN_FILENO, &seq[0], 1) != 1) {
      return '\x1b';
    }
    if(read(STDIN_FILENO, &seq[1], 1) != 1) {
      return '\x1b';
    }
    if(seq[0] == '[') { // Checking to see if the escape key is an special or arrow key escape sequence, not just escape
      if(seq[1] >= '0' && seq[1] <= '9') {
        if(read(STDIN_FILENO, &seq[2], 1) != 1) {
          return '\x1b';
        }
        if(seq[2] == '~') {
          switch(seq[1]) {
            case '1':
              return HOME_KEY;
            case '3':
              return DEL_KEY;
            case '4':
              return END_KEY;
            case '5':
              return PAGE_UP; // \x1b[5~
            case '6':
              return PAGE_DOWN; // \x1b[6~
            case '7':
              return HOME_KEY;
            case '8':
              return END_KEY;
          }
        }
      }
      else {
        switch(seq[1]) { // Aliasing the arrow keys
          case 'A':
            return ARROW_UP;
          case 'B':
            return ARROW_DOWN;
          case 'C':
            return ARROW_RIGHT;
          case 'D':
            return ARROW_LEFT;
          case 'H':
            return HOME_KEY;
          case 'F':
            return END_KEY;
        }
      }
    }
    else if(seq[0] == 'O') {
      switch(seq[1]) {
        case 'H':
          return HOME_KEY;
        case 'F':
          return END_KEY;
      }
    }
    return '\x1b';
  }
  else {
    return c;
  }
}

int getCursorPosition(int *rows, int* cols) {
  char buf[32];
  unsigned int i = 0;
  if(write(STDOUT_FILENO, "\x1b[6n", 4) != 4) { // n command with argument of 6 gets cursor position
    return -1;
  }
  while(i < sizeof(buf) -1) { // Reading characters from the terminal response into buf
    if(read(STDIN_FILENO, &buf[i], 1) != 1) { // Reading one character at a time
      break;
    }
    if(buf[i] == 'R') { // Terminal response always ends with the letter R
      break;
    }
    i++;
  }
  buf[i] = '\0'; // printf() expects strings to end with a 0 byte
  if(buf[0] != '\x1b' || buf[1] != '[') { // Makes sure it responded with an escape sequence
    return -1;
  }
  if(sscanf(&buf[2], "%d;%d", rows, cols) != 2) { // Parses integers separately from string into rows and cols
    return -1;
  }
  // printf("\r\n&buf[1]: '%s'\r\n", &buf[1]); // Skips first character (escape sequence not displayable) with &buf[1]
  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    // Using escape sequences to get cursor position if ioctl() does not work on the system being used
    if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) { // C is right, B is down, and value 999 ensures it reaches bottom right corner of the screen
      return getCursorPosition(rows, cols);
    }
    editorReadKey(); // To observe result of escape sequences before caalling die()
    return -1;
  }
  else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** syntax highlighting ***/

int isSeparator(int c) {
  // strchr returns pointer to first occurrence of a matching character
  return isspace(c) || c == '\0' || strchr(",.()+-/" "*" "=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(editorRow *row) { // Goes through characters in an editorRow and highlights them
  row->hl = realloc(row->hl, row->rSize); // This may be a new row or the row may be bigger than last time
  memset(row->hl, HL_NORMAL, row->rSize); // hl is same size as render, sets all characters to HL_NORMAL by default
  if(E.syntax == NULL) { // No filetype set
    return;
  }
  char **keywords = E.syntax->keywords;
  char *slcs = E.syntax->singleLineCommentStart;
  char *mlcs = E.syntax->multiLineCommentStart;
  char *mlce = E.syntax->multiLineCommentEnd;
  int slcsLen = slcs ? strlen(slcs) : 0; // Length of string or 0
  int mlcsLen = mlcs ? strlen(mlcs) : 0;
  int mlceLen = mlce ? strlen(mlce) : 0;
  int prevSep = 1; // Keeps track of whether previous character was a separator (beginning of line is initially true)
  int inString = 0; // Keep track of whether we are inside a string
  int inComment = (row->idx > 0 && E.rows[row->idx - 1].hlOpenComment); // True if previous row has unclosed multi-line comment
  int i = 0;
  while(i < row->rSize) { // Multiple characters each iteration
    char c = row->render[i];
    unsigned char prevHl = (i > 0) ? row->hl[i - 1] : HL_NORMAL; // Set to highlight type of previous character
    if(slcsLen && !inString && !inComment) { // Not in a string or a multi-line comment
      if(!strncmp(&row->render[i], slcs, slcsLen)) { // Checks if character is start of single-line comment
        memset(&row->hl[i], HL_COMMENT, row->rSize - i);
        break;
      }
    }
    if(mlcsLen && mlceLen && !inString) { // Non-NULL strings of length greater than 0 and not in a string
      if(inComment) {
        row->hl[i] = HL_MLCOMMENT; // Highlight character
        if(!strncmp(&row->render[i], mlce, mlceLen)) { // Check if at end of a multi-line comment
          memset(&row->hl[i], HL_MLCOMMENT, mlceLen);
          i += mlceLen; // Consume whole string
          inComment = 0;
          prevSep = 1;
          continue;
        }
        else {
          i++; // Consume current character if not at the end of the comment
          continue;
        }
      }
      else if(!strncmp(&row->render[i], mlcs, mlcsLen)) { // Check if at beginning of multi-line comment
        memset(&row->hl[i], HL_MLCOMMENT, mlcsLen);
        i += mlcsLen;
        inComment = 1;
        continue;
      }
    }
    if(E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if(inString) { // Current character can be highlighted
        row->hl[i] = HL_STRING;
        if(c == '\\' && i + 1 < row->rSize) { // Escape quotes -> highlights character in string after backslash
          row->hl[i + 1] = HL_STRING;
          i += 2; // Consume both characters at once
          continue;
        }
        if(c == inString) { // Checks if current character is the closing quote
          inString = 0;
        }
        i++;
        prevSep = 1; // Done highlighting the string
        continue;
      }
      else {
        if(c == '"' || c == '\'') { // Checking if at beginning of a string
          inString = c;
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }
    if(E.syntax->flags & HL_HIGHLIGHT_NUMBERS) { // Should numbers be highlighted for current filetype
      if((isdigit(c) && (prevSep || prevHl == HL_NUMBER)) || // Previous character must either be a separator or also highlighted with HL_NUMBER
         (c == '.' && prevHl == HL_NUMBER)) { // Numbers with decimal points
        row->hl[i] = HL_NUMBER;
        i++; // Consumes that character
        prevSep = 0; // In the middle of highlighting something
        continue;
      }
    }
    if(prevSep) { // Make sure a separator came before keyword, to work around words being highlighted when they should not
      for(int j = 0; keywords[j]; j++) { // Looping through keywords
        int kLen = strlen(keywords[j]); // Length of keyword being looked at
        int kw2 = keywords[j][kLen - 1] == '|'; // Secondary keyword
        if(kw2) {
          kLen--; // Remove length to account for the the pipe character
        }
        if(!strncmp(&row->render[i], keywords[j], kLen) && // Check if keyword exists at position in the text
           isSeparator(row->render[i + kLen])) { // Checks if separator comes after keyword
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, kLen); // Keyword highlighted based on value of kw2
          i+= kLen; // Consume entire keyword
          break;
        }
      }
      if(keywords[i] != NULL) { // Check if loop was broken out of by seeing if it got the null terminator
        prevSep = 0;
        continue;
      }
    }
    prevSep = isSeparator(c);
    i++;
  }
  int changed = (row->hlOpenComment != inComment);
  row->hlOpenComment = inComment; // Set current row value that inComment got left in after processing the row
  if(changed && row->idx + 1 < E.numRows) { // Check if changed and there is a next line in the file
    editorUpdateSyntax(&E.rows[row->idx + 1]); // Change propagates to more and more lines until one is unchanged
  }
}

int editorSyntaxToColour(int hl) {
  switch(hl) { // Returns ANSI codes
    case HL_COMMENT:
    case HL_MLCOMMENT:
      return 36; // Cyan
    case HL_KEYWORD1:
      return 33; // Yellow
    case HL_KEYWORD2:
      return 32; // Green
    case HL_STRING:
      return 35; // Magenta
    case HL_NUMBER:
      return 31; // Foreground red
    case HL_MATCH:
      return 34; // Blue
    default:
      return 37; // Foreground white
  }
}

void editorSelectSyntaxHighlight() { // Match filename to fileMatch fields in HLDB
  E.syntax = NULL;
  if(E.fileName == NULL) {
    return;
  }
  char *ext = strrchr(E.fileName, '.'); // Returns pointer to last occurrence of a character in a string
  for(unsigned int i = 0; i < HLDB_ENTRIES; i++) {
    struct editorSyntax *s = &HLDB[i];
    unsigned int j = 0;
    while(s->fileMatch[j]) {
      int isExt = (s->fileMatch[j][0] == '.'); // Pattern in fileMatch is an extension
      if((isExt && ext && !strcmp(ext, s->fileMatch[j])) || // Checks if fileName ends with extension
         (!isExt && strstr(E.fileName, s->fileMatch[j]))) { // Otherwise checks if pattern exists in filename
        E.syntax = s;
        for(int fileRow = 0; fileRow < E.numRows; fileRow++) { // Highlighting immediately changes with filetype
          editorUpdateSyntax(&E.rows[fileRow]);
        }
        return;
      }
      j++;
    }
  }
}

/*** row operations ***/

int editorRowCxToRx(editorRow *row, int cx) {
  int rx = 0;
  for(int i = 0; i < cx; i++) {
    if(row->chars[i] == '\t') {
      rx += (TAB_STOP - 1) - (rx % TAB_STOP); // Number of columns to right of last tab stop, subtracted from number left of the next
    }
    rx++; // Moves right on the next tab stop
  }
  return rx;
}

int editorRowRxToCx(editorRow *row, int rx) {
  int rxCur = 0;
  int cx;
  for(cx = 0; cx < row->size; cx++) {
    if(row->chars[cx] == '\t') {
      rxCur += (TAB_STOP - 1) - (rxCur % TAB_STOP);
    }
    rxCur++;
    if(rxCur > rx) {
      return cx;
    }
  }
  return cx;
}

void editorUpdateRow(editorRow *row) {
  int tabs = 0;
  for(int i = 0; i < row->size; i++) {
    if(row->chars[i] == '\t') {
      tabs++; // Counting the number of tabs in the chars of the row
    }
  }
  free(row->render);
  // Maximum number of characters for a tab is 8 (row->size counts for 1 each tab)
  row->render = malloc(row->size + tabs * (TAB_STOP - 1) + 1);
  int idx = 0;
  for(int i = 0; i < row->size; i++) {
    if(row->chars[i] == '\t') { // Checks if current character is tab
      row->render[idx++] = ' '; // Append a space
      while(idx % TAB_STOP != 0) { // Append spaces until a tab stop (column divisible by 8)
        row->render[idx++] = ' ';
      }
    }
    else {
      row->render[idx++] = row->chars[i]; // Copy from chars to render
    }
  }
  row->render[idx] = '\0';
  row->rSize = idx; // idx contains number of characters copied
  editorUpdateSyntax(row); // After updating render
}

void editorInsertRow(int at, char *s, size_t len) {
  if(at < 0 || at > E.numRows) {
    return;
  }
  E.rows = realloc(E.rows, sizeof(editorRow) * (E.numRows + 1)); // Bytes each editorRow takes multiplied by number of rows
  memmove(&E.rows[at + 1], &E.rows[at], sizeof(editorRow) * (E.numRows - at)); // Make room at index for new row
  for(int i = at + 1; i <= E.numRows; i++) { // Updating indexes whenever a row is inserted
    E.rows[i].idx++;
  }
  E.rows[at].idx = at;
  E.rows[at].size = len;
  E.rows[at].chars = malloc(len + 1);
  memcpy(E.rows[at].chars, s, len);
  E.rows[at].chars[len] = '\0';
  E.rows[at].rSize = 0;
  E.rows[at].render = NULL;
  E.rows[at].hl = NULL;
  E.rows[at].hlOpenComment = 0;
  editorUpdateRow(&E.rows[at]);
  E.numRows++;
  E.dirty++;
}

void editorFreeRow(editorRow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

void editorDelRow(int at) {
  if(at < 0 || at >= E.numRows) {
    return;
  }
  editorFreeRow(&E.rows[at]);
  memmove(&E.rows[at], &E.rows[at + 1], sizeof(editorRow) * (E.numRows - at - 1)); // Overwrite deleted row with next
  for(int i = at; i < E.numRows - 1; i++) { // Updating indexes whenever a row is deleted
    E.rows[i].idx--;
  }
  E.numRows--;
  E.dirty++;
}

void editorRowInsertChar(editorRow *row, int at, int c) {
  if(at < 0 || at > row->size) { // at is the index the character is inserted into
    at = row->size; // If past the end of string, it is inserted at the end of the string
  }
  row->chars = realloc(row->chars, row->size + 2); // Allocate one byte for the chars, and another because of null byte
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1); // Like memcpy(), but safe when arrays overlap
  row->size++; // Increment size of chars array
  row->chars[at] = c; // Assign character to position in array
  editorUpdateRow(row); // render and rSize fields get updated with the new row content
  E.dirty++;
}

void editorRowAppendString(editorRow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1); // row is what we are appending to, and s is being appended
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(editorRow *row, int at) {
  if(at < 0 || at >= row->size) {
    return;
  }
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at); // Move characters that come after it back
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if(E.cy == E.numRows) { // Cursor is on the tilde line before end of the file
    editorInsertRow(E.numRows, "", 0); // Create a new row in that case
  }
  editorRowInsertChar(&E.rows[E.cy], E.cx, c); // Insert character
  E.cx++; // Move cursor to the right for the next character
}

void editorInsertNewline() {
  if(E.cx == 0) { // If at the beginning of a line, insert a new blank row before the current line
    editorInsertRow(E.cy, "", 0);
  }
  else { // Otherwise split into two rows
    editorRow *row = &E.rows[E.cy]; // Current row
    // Passing characters to right of cursor to editorInsertRow(), creating new row with those characters on it
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.rows[E.cy]; // Reassigns because realloc() in editorInsertRow() may move around memory
    row->size = E.cx; // Truncates row contnets
    row->chars[row->size] = '\0';
    editorUpdateRow(row); // editorInsertRow() updates new row
  }
  E.cy++;
  E.cx = 0; // Cursor to beginning of row
}

void editorDelChar() {
  if(E.cy == E.numRows) { // If past the end of file, there is nothing to delete
    return;
  }
  if(E.cx == 0 && E.cy == 0) { // Beginning of first line
    return;
  }
  editorRow *row = &E.rows[E.cy]; // Gets the editorRow the cursor is on
  if(E.cx > 0) { // If there is a character to the left of the cursor
    editorRowDelChar(row, E.cx - 1); // Delete it
    E.cx--; // Move cursor one to the left
  }
  else { // Cursor at beginning of a line
    E.cx = E.rows[E.cy - 1].size; // End of contents of the previous row
    editorRowAppendString(&E.rows[E.cy - 1], row->chars, row->size); // Append contents to previous row
    editorDelRow(E.cy);
    E.cy--;
  }
}


/*** file i/o ***/

char *editorRowsToString(int *bufLen) {
  int totLen = 0;
  for(int i = 0; i < E.numRows; i++) {
    totLen += E.rows[i].size + 1; // Adding up lengths of each row of text, + 1 for each newline character
  }
  *bufLen = totLen; // Length of string
  char *buf = malloc(totLen);
  char *p = buf;
  for(int i = 0; i < E.numRows; i++) {
    memcpy(p, E.rows[i].chars, E.rows[i].size); // Copy contents of each row to the end of the buffer
    p += E.rows[i].size;
    *p = '\n'; // Appending newline character after each row
    p++;
  }
  return buf; // Single string to be written out to a file
}

void editorOpen(char *fileName) { // Reading file from disk
  free(E.fileName);
  E.fileName = strdup(fileName); // Makes a copy of the string
  editorSelectSyntaxHighlight(); // E.fileName changes
  FILE *fp = fopen(fileName, "r");
  if(!fp) {
    die("fopen");
  }
  char *line = NULL;
  size_t lineCap = 0;
  ssize_t lineLen;
  while((lineLen = getline(&line, &lineCap, fp)) != -1) { // lineLen being -1 means the end of the file and no more lines to read
    while(lineLen > 0 && (line[lineLen - 1] == '\n' || line[lineLen - 1] == '\r')) {
      lineLen--;
    }
    editorInsertRow(E.numRows, line, lineLen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0; // editorAppendRow() is called, incrementing E.dirty, so this resets it to 0
}

void editorSave() {
  if(E.fileName == NULL) {
    E.fileName = editorPrompt("SAVE AS: %s (ESC TO CANCEL)", NULL);
    if(E.fileName == NULL) {
      editorSetStatusMessage("SAVE ABORTED");
      return;
    }
    editorSelectSyntaxHighlight(); // E.fileName changes
  }
  int len;
  char *buf = editorRowsToString(&len);
  // O_RDWR means open for reading and writing, and 0644 is the standard permissions mode for text files
  int fd = open(E.fileName, O_RDWR | O_CREAT, 0644); // Creates a new file if it does not exist
  if(fd != -1) {
    if(ftruncate(fd, len) != -1) { // Sets file size to specified length, and cuts off any data at the end of the file
      if(write(fd, buf, len) == len) { // Writes the string to the path in E.fileName
        close(fd);
        free(buf);
        E.dirty = 0; // No new changes
        editorSetStatusMessage("%d BYTES WRITTEN TO DISK", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  editorSetStatusMessage("UNABLE TO SAVE FILE - I/O error: %s", strerror(errno)); // Human-readable string for the error
}

/*** find ***/

void editorFindCallback(char *query, int key) { // Callback for incremental search
  static int lastMatch = -1;
  static int direction = 1;
  static int savedHlLine; // Line to be restored
  static char *savedHl = NULL;
  if(savedHl) {
    memcpy(E.rows[savedHlLine].hl, savedHl, E.rows[savedHlLine].rSize);
    free(savedHl); // Guaranteed to be call free() on savedHl before malloc() is called on it
    savedHl = NULL;
  }
  if(key == '\r' || key == '\x1b') { // Leaving search
    lastMatch = -1;
    direction = 1;
    return;
  }
  else if(key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1; // Forward
  }
  else if(key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1; // Backward
  }
  else {
    lastMatch = -1; // Always set to -1 unless an arrow key is pressed
    direction = 1;
  }
  if(lastMatch == -1) {
    direction = 1;
  }
  // If there was a lastMatch, it starts on line after or before
  // If no lastMatch, it starts at top of file and searches in forward direction until first match
  int current = lastMatch; // Index of current row we are searching
  for(int i = 0; i < E.numRows; i++) {
    current += direction;
    if(current == -1) { // Causes current to go from end of the file back to beginning
      current = E.numRows - 1;
    }
    else if(current == E.numRows) {
      current = 0;
    }
    editorRow *row = &E.rows[current];
    char *match = strstr(row->render, query); // Checks if query is a substring of the current row
    if(match) { // Match is a pointer into the row->render string
      lastMatch = current; // Next search started from this point
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.rowOff = E.numRows; // editorScroll() scrolls upwards at next screen refresh so match is at the top
      savedHlLine = current;
      savedHl = malloc(row->rSize);
      memcpy(savedHl, row->hl, row->rSize);
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query)); // match - row->render is index into render and hl here
      break;
    }
  }
}

void editorFind() {
  // Returning cursor back to where it was before search
  int cxSaved = E.cx;
  int cySaved = E.cy;
  int colOffSaved = E.colOff;
  int rowOffSaved = E.rowOff;
  char *query = editorPrompt("SEARCH: %s (USE ARROWS/ESC/ENTER)", editorFindCallback);
  if(query) {
    free(query);
  }
  else {
    E.cx = cxSaved;
    E.cy = cySaved;
    E.colOff = colOffSaved;
    E.rowOff = rowOffSaved;
  }
}


/*** append buffer ***/

// Replacing all write() calls with code that appends the string to a buffer
struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0} // Constructor for an empty buffer

void abAppend(struct abuf *ab, const char *s, int len) {
  char *new = realloc(ab->b, ab->len + len); // Gives block of memory the size of the string plus the string being appended
  if(new == NULL) {
    return;
  }
  memcpy(&new[ab->len], s, len); // Copies string s after current data ends into the buffer
  ab->b = new; // Updates pointer
  ab->len += len; // Updates length
}

void abFree(struct abuf *ab) { // Destructor
  free(ab->b);
}

/*** output ***/

void editorScroll() {
  E.rx = 0;
  if(E.cy < E.numRows) {
    E.rx = editorRowCxToRx(&E.rows[E.cy], E.cx);
  }
  // Vertical
  if(E.cy < E.rowOff) { // Checks if cursor is above visible window
    E.rowOff = E.cy; // Scrolls to where the cursor is
  }
  if(E.cy >= E.rowOff + E.screenRows) { // Checks if past the bottom of the visible window
    E.rowOff = E.cy - E.screenRows + 1; // Cursor is at the bottom of the screen
  }
  // Horizontal
  if(E.rx < E.colOff) { // Checks if on the visible left side of the screen
    E.colOff = E.rx; // Scrolls to where the cursor is
  }
  if(E.rx >= E.colOff + E.screenCols) { // Checks if past the right side of the screen
    E.colOff = E.rx - E.screenCols + 1; // Cursor is at the right side of the screen
  }
}

void editorDrawRows(struct abuf *ab) { // Handles drawing each row of the buffer of text being edited
  int y;
  for(y = 0; y < E.screenRows; y++) {
    int fileRow = y + E.rowOff;
    if(fileRow >= E.numRows) { // Row is after the end of the text buffer
      if(E.numRows == 0 && y == E.screenRows / 3) { // Only displays welcome message if the text buffer is empty
        char welcome[80];
        int welcomeLen = snprintf(welcome, sizeof(welcome),
          "Lyte editor -- version %s", LYTE_VERSION);
        if(welcomeLen > E.screenCols) { // Truncates length of string
          welcomeLen = E.screenCols;
        }
        int padding = (E.screenCols - welcomeLen) / 2;
        if(padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while(padding--) {
          abAppend(ab, " ", 1);
        }
        abAppend(ab, welcome, welcomeLen);
      }
      else {
        abAppend(ab, "~", 1);
      }
    }
    else { // Row is part of the text buffer
      int len = E.rows[fileRow].rSize - E.colOff; // Subtracts number of characters to the left from length of row
      if(len < 0) { // Prevents scrolling past the end of the line
        len = 0;
      }
      if(len > E.screenCols) { // Truncates rendered line
        len = E.screenCols;
      }
      char *c = &E.rows[fileRow].render[E.colOff];
      unsigned char *hl = &E.rows[fileRow].hl[E.colOff]; // Slice of hl array corresponding to slice of render
      int currentColour = -1; // Only print escape sequence when colour changes
      for(int i = 0; i < len; i++) {
        if(iscntrl(c[i])) { // Checks if control character
          char sym = (c[i] <= 26) ? '@' + c[i] : '?'; // Translates to printable by adding @ (capitals come after @ in ASCII)
          abAppend(ab, "\x1b[7m", 4); // Switch to inverted colours before printing translated symbol
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3); // Turns off all text formatting
          if(currentColour != -1) {
            char buf[16];
            int cLen = snprintf(buf, sizeof(buf), "\x1b[%dm", currentColour); // Printing escape sequence for current colour
            abAppend(ab, buf, cLen);
          }
        }
        else if(hl[i] == HL_NORMAL) { // Have to feed character-by-character into abAppend() due to highlighting
          // Default -> colour is -1
          if(currentColour != -1) {
            abAppend(ab, "\x1b[39m", 5); // Setting text colour with ANSI escape codes as VT100 does not document colour
            currentColour = -1;
          }
          abAppend(ab, &c[i], 1);
        }
        else {
          int colour = editorSyntaxToColour(hl[i]);
          if(colour != currentColour) {
            currentColour = colour;
            char buf[16];
            int cLen = snprintf(buf, sizeof(buf), "\x1b[%dm", colour); // Write escape sequence into buffer
            abAppend(ab, buf, cLen); // Pass buffer to abAppend()
          }
          abAppend(ab, &c[i], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }
    abAppend(ab, "\x1b[K", 3); // Erases part of line to the right of the cursor (more optimal for screen refresh)
    abAppend(ab, "\r\n", 2); // Status bar is final line
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  abAppend(ab, "\x1b[7m", 4); // Inverts colour (m is graphic rendition, 7 is inverted colours)
  char status[80], rStatus[80];
  int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", // 20 characters of file name
    E.fileName ? E.fileName : "[No Name]", E.numRows, E.dirty ? "(modified)" : "");
  int rLen = snprintf(rStatus, sizeof(rStatus), "%s | %d/%d",
    E.syntax ? E.syntax -> fileType : "no filetype", E.cy + 1, E.numRows); // Current line
  if(len > E.screenCols) {
    len = E.screenCols;
  }
  abAppend(ab, status, len);
  while(len < E.screenCols) {
    if(E.screenCols - len == rLen) { // The point where the second status string is against right edge of screen
      abAppend(ab, rStatus, rLen);
      break;
    }
    else {
      abAppend(ab, " ", 1); // Blank bar
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3); // Sets back to normal formatting
  abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3); // Clears message
  int msgLen = strlen(E.statusMsg);
  if(msgLen > E.screenCols) {
    msgLen = E.screenCols;
  }
  if(msgLen && time(NULL) - E.statusMsgTime < 5) { // Only displays message if less than 5 seconds old
    abAppend(ab, E.statusMsg, msgLen);
  }
}

void editorRefreshScreen() {
  editorScroll();
  struct abuf ab = ABUF_INIT;
  abAppend(&ab, "\x1b[?25l", 6); // Resets cursor
  // abAppend(&ab, "\x1b[2J", 4); // Writes an escape sequence to the terminal, clearing the entire screen
  abAppend(&ab, "\x1b[H", 3); // Repositions the cursor at the top-left corner using H command
  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);
  char buf[32];
  // E.cy refers to the position of the cursor within the text file, so rowOff must be subtracted to refer to the screen
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowOff) + 1, (E.rx - E.colOff) + 1); // Positions the cursor with 1-indexed arguments
  abAppend(&ab, buf, strlen(buf));
  abAppend(&ab, "\x1b[?25h", 6); // Sets cursor
  write(STDOUT_FILENO, ab.b, ab.len); // Writes buffer content to standaard output
  abFree(&ab); // Frees memory used by the abuf
}

void editorSetStatusMessage(const char *fmt, ...) { // Variadic function (any number of arguments)
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusMsg, sizeof(E.statusMsg), fmt, ap); // Resulting string stored in statusMessage
  va_end(ap);
  E.statusMsgTime = time(NULL); // Current time
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize); // Input stored in buf
  size_t bufLen = 0;
  buf[0] = '\0';
  while(1) { // Repeatedly sets status message, refreshes screen, and waits for a keypress to handle
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();
    int c = editorReadKey();
    if(c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if(bufLen != 0) {
        buf[--bufLen] = '\0';
      }
    }
    else if(c == '\x1b') { // Escape key to cancel prompt
      editorSetStatusMessage("");
      if(callback) { // Allows user to pass NULL for the callback
        callback(buf, c);
      }
      free(buf);
      return NULL;
    }
    else if(c == '\r') {
      if(bufLen != 0) { // User presses enter, and input not empty
        editorSetStatusMessage("");
        if(callback) {
          callback(buf, c);
        }
        return buf;
      }
    }
    else if (!iscntrl(c) && c < 128) { // c < 128 checks character is in range of a char, not a special key
      if(bufLen == bufsize - 1) { // If buflen has reached maximum allocated capacity
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[bufLen++] = c; // Appends printable character to buf
      buf[bufLen] = '\0'; // Null terminator indicating string has ended
    }
    if(callback) {
      callback(buf, c);
    }
  }
}

void editorMoveCursor(int key) {
  // If cursor is on an actual line, then row points to the editorRow that the cursor is on
  editorRow *row = (E.cy >= E.numRows) ? NULL : &E.rows[E.cy];
  switch(key) {
    case ARROW_UP:
      if(E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_LEFT:
      if(E.cx != 0) {
        E.cx--;
      }
      else if(E.cy > 0) { // Left onto previous line
        E.cy--;
        E.cx = E.rows[E.cy].size;
      }
      break;
    case ARROW_DOWN:
      if(E.cy != E.numRows) {
        E.cy++;
      }
      break;
    case ARROW_RIGHT:
      if(row && E.cx < row->size) { // Checks if E.cx is to the left of the end of that line before moving right
        E.cx++;
      }
      else if(row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
  }
  row = (E.cy >= E.numRows) ? NULL : &E.rows[E.cy]; // Set again as E.cy may point to a different line
  int rowLen = row ? row-> size : 0;
  if(E.cx > rowLen) {
    E.cx = rowLen; // Set to the end of that line
  }
}

void editorProcessKeypress() { // Handles keypresses
  static int quitTimes = QUIT_TIMES;
  int c = editorReadKey();
  switch(c) { // Maps Ctrl key combinations and special keys to editor functions
    case '\r': // Enter
      editorInsertNewline();
      break;
    case CTRL_KEY('q'):
      // Clears screen on exit
      if(E.dirty && quitTimes > 0) {
        editorSetStatusMessage("WARNING - FILE HAS UNSAVED CHANGES: PRESS CTRL-Q %d MORE TIMES TO QUIT", quitTimes);
        quitTimes--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);
      break;
    case CTRL_KEY('s'):
      editorSave();
      break;
    case HOME_KEY: // Left of screen
      E.cx = 0;
      break;
    case END_KEY: // Right of screen
      if(E.cy < E.numRows) {
        E.cx = E.rows[E.cy].size; // Brings cursor to the end of the current line, and if none, E.cx will be 0
      }
      break;
    case CTRL_KEY('f'):
      editorFind();
      break;
    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
      if(c == DEL_KEY) { // Deletes the character to the right of the cursor
        editorMoveCursor(ARROW_RIGHT);
      }
      editorDelChar();
      break;
    case PAGE_UP: // Top of screen
    case PAGE_DOWN: // Bottom of screen
      {
        if(c == PAGE_UP) {
          E.cy = E.rowOff;
        }
        else if(c == PAGE_DOWN) {
          E.cy = E.rowOff + E.screenRows - 1;
          if(E.cy > E.numRows) {
            E.cy = E.numRows;
          }
        }
        int times = E.screenRows;
        while(times--) {
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        }
      }
      break;
    case ARROW_UP:
    case ARROW_LEFT:
    case ARROW_DOWN:
    case ARROW_RIGHT:
      editorMoveCursor(c);
      break;
    case CTRL_KEY('l'):
    case '\x1b':
      break;
    default:
      editorInsertChar(c);
      break;
  }
  quitTimes = QUIT_TIMES;
}

/*** init ***/

void initEditor() { // Initialises all fields in E struct
  E.cx = 0; // Horizontal cursor coordinate (column)
  E.cy = 0; // Vertical cursor coordinate (row)
  E.rx = 0; // If there are tabs, E.rx will be greater than E.cx by the number of extra spaces those tabs take up
  E.rowOff = 0;
  E.colOff = 0;
  E.numRows = 0;
  E.rows = NULL;
  E.dirty = 0;
  E.fileName = NULL;
  E.statusMsg[0] = '\0';
  E.statusMsgTime = 0;
  E.syntax = NULL; // No filetype initially -> no syntax highlighting
  if(getWindowSize(&E.screenRows, &E.screenCols) == -1) {
    die("getWindowSize");
  }
  E.screenRows -= 2; // Status and message bars at the bottom of the screen
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();
  if(argc >= 2) {
    editorOpen(argv[1]);
  }
  editorSetStatusMessage("HELP: CTRL-S = SAVE | CTRL-Q = QUIT | CTRL-F = FIND");
  while(1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }
  return 0;
}
