#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct cd {
    char artist[512];
    char album[512];
    double price;
    struct cd *next;
};

struct cd *newCD(char artist[512], char album[512], double price);
struct cd *readFromFile(char *filealbum);
double addPrices(struct cd *head);
int numberOfCDs(struct cd *head);



int main(int argc, char *argv[]){
    struct cd *head = readFromFile("/Users/Jack/Sync/Misc/CD Wishlist.txt");
    float price = addPrices(head);
    float discount = price * 0.05;
    price = price - discount;
    printf("-£%.2lf\t**Student Discount**\n", discount);
    printf("Total for %d CDs:\t£%.2lf\n\n", numberOfCDs(head), price);
};

struct cd *newCD(char artist[512], char album[512], double price){
    struct cd *c = (struct cd *)malloc(sizeof(struct cd)); //allocates the right amount of memory for a new struct
    if(c != NULL) {
        strcpy(c->artist, artist);
        strcpy(c->album, album);
        c->price = price;
        c->next = NULL;
    }
    return c;
}

struct cd *readFromFile(char *filealbum){
    char line[512];
    char artist[512];
    char album[512];
    double price;
    struct cd *tmp = newCD("", "",  0);
    FILE *data = fopen(filealbum, "r");
    if(data != NULL){
        while(fgets(line,512,data) != NULL){
            sscanf(line, "%lf\t%[^-] - %[^\n]\n", &price, artist, album);       //Note: all CDs will have a trailing space after the artist name
            struct cd *new = newCD(artist, album, price);
            new->next = tmp;
            tmp = new;
        }
    } else {
        printf("Error: file is NULL\n");
        exit(1);
    }
    fclose(data);
    return tmp;
}

double addPrices(struct cd *head){
    double total = 0;
    printf("\nPrice\tArtist - Album\n----------------------\n");
    while(head != NULL){
        if(head->price > 0){
            total += head->price;
            printf("£%.2lf\t%s- %s\n", head->price, head->artist, head->album);
            head = head->next;
        } else {
            head = head->next;
        }
    }
    return total;
}

int numberOfCDs(struct cd *head){
    int num = 0;
    while(head != NULL){
        if(head->price > 0){
            num++;
            head = head->next;
        } else {
            head = head->next;
        }
    }
    return num;
}

