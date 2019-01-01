#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define BUF_SIZE 100
char* prompt = "xlex> ";
char buffer[BUF_SIZE];
int pointer = 0;
void recognize();
void printRecognizing(int from);
void printNotRecognizing(int from);

char getSymbol() {
  return buffer[pointer++];
}

void retract() {
  pointer--;
}

int main(int argc, char **argv) {
  while (1) {
    pointer = 0;
    printf("%s", prompt);
    char* result = fgets(buffer, sizeof(buffer), stdin);
    if (result == NULL) break;
    while (1) {
      int recordPtrPos = pointer;
      recognize();
      if (recordPtrPos == pointer) { // pointer cannot move forward
        if (buffer[pointer] == '\n' || buffer[pointer] == '\0') break;
        printNotRecognizing(pointer);
        break;
      } else printRecognizing(recordPtrPos);
    }
  }
  printf("\n");
  return 0;
}

void printRecognizing(int from) {
  printf("recognized: ");
  while (from < pointer) {
    putchar(buffer[from]);
    from++;
  }
  printf("\n");
}

void printNotRecognizing(int from) {
  char* notRecogMsg = "not recognize: ";
  printf("%s%s", notRecogMsg, buffer);
  int indent = strlen(notRecogMsg) + from;
  if (indent > 0) {
    char* blank = (char*)malloc(sizeof(char) * (indent + 1));
    if (blank == NULL) {
      printf("Unable to malloc\n");
      exit(1);
    }
    for (int i = 0; i != indent; ++i) blank[i] = '~';
    blank[indent] = '\0';
    printf("%s", blank);
    free(blank);
  }
  printf("^\n");
}

void recognize() {
  int stateId = 1;
  int timeToExit = 0;
  while (!timeToExit) {
    char ch = getSymbol();
    switch (stateId) {
    case 0:
      if (ch == 'a') stateId = 2;
      else if (ch == 'b') stateId = 1;
      else {
        retract();
        timeToExit = 1;
      }
      break;
    case 1:
      if (ch == 'a') stateId = 2;
      else if (ch == 'b') stateId = 1;
      else {
        retract();
        timeToExit = 1;
      }
      break;
    case 2:
      if (ch == 'a') stateId = 2;
      else if (ch == 'b') stateId = 3;
      else {
        retract();
        timeToExit = 1;
      }
      break;
    case 3:
      if (ch == 'a') stateId = 2;
      else if (ch == 'b') stateId = 0;
      else {
        retract();
        timeToExit = 1;
      }
      break;
    }
  }
}
