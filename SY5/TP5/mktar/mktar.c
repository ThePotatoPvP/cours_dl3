#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <libgen.h>
#include <sys/param.h>
#include <errno.h>
#include <sys/dir.h>
#include <dirent.h>

#include "tarutils.h"
#include "mystring.h"

char zeros[BLOCKSIZE] = {0};
char buf[BLOCKSIZE];
struct posix_header hd;

// 1. Un bloc est nécessaire et suffisant.

int tar_file(int fd, char * filename) {
  int n, in;
  struct stat st;
  struct posix_header * phd = &hd;
  memset(phd, 0, BLOCKSIZE);
  stat(filename, &st);
  strcpy(phd -> name, filename);
  sprintf(phd -> mode,"%07o", st.st_mode & 0777);

  /* TODO[1] modifier la suite de la fonction `tar_file` afin quelle
   * puisse archiver des fichiers ordinaires et des répertoires. */

  if (S_ISREG(st.st_mode)) {
    phd -> typeflag = REGTYPE;
    sprintf(phd -> size,"%011lo", st.st_size);}
  else{
    fprintf(stderr, "Erreur : type de fichier non supporté\n");
    return 1;}
  set_checksum(phd);
  n = write(fd, phd, BLOCKSIZE);
  if (n < 0) {
    perror("Erreur d'écriture de l'entête ");
    return 1;}

  if (S_ISDIR(st)){
    return 0;
  }

  in = open(filename, O_RDONLY);

  if (in < 0) {
    perror("Erreur à l'ouverture du fichier à archiver");
    return 1;}
  while ((n = read(in, buf, BLOCKSIZE)) == BLOCKSIZE) {
    write(fd, buf, BLOCKSIZE);}
  if (n != 0) {
    write(fd, buf, n);
    write(fd, zeros, BLOCKSIZE-n);}
  close(in);
  return 0;
}


int process(struct string * path,int fd){
  struct stat st; int tmp;
  DIR * dir = NULL;
  if (stat(path->data,&st) < 0) {goto error;}

  /* TODO[2] modifier la fonction process pour que, si path désigne un
   * répertoire, elle l'archive et appelle récursivement process sur les
   * fichiers contenu dans le répertoire. */

  tmp=tar_file(fd, path->data);
  if (tmp!=0){ goto error;}

  struct dirent* entry;

  if (S_ISDIR(st)){
    DIR *dp = opendir(path->data);

    // Vérifie si l'ouverture du répertoire a réussi
    if (dp == NULL) {
      perror("Erreur lors de l'ouverture du répertoire");
      goto error;
    }

    while (((entry = readdir(dir)) != NULL) && (entry->d_name[0] != '.')){
      string_append(path, "/");
      string_append(path, entry->d_name);
      process(path, fd);
      string_truncate(path, strlen(entry->d_name)+1);
    }
  }

  return 0;

  error:
    if (errno){perror("process");}
    if (dir) closedir(dir);
    return -1;}

int main(int argc, char **argv){
  int fd, i;
  struct string * path = NULL;
  if (argc < 3) {
    fprintf(stderr, "Usage : %s file.tar f1 [... fn]\n", argv[0]);
    exit(1);}
  fd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC,0644);
  if (fd < 0) {
    perror("Erreur à l'ouverture de l'archive");
    exit(1);}
  for (i = 2; i < argc; i++) {
    path = string_new(PATH_MAX);
    if (!string_append(path, argv[i])) { goto error; }
    if (process(path,fd)!=0) {
      fprintf(stderr, "Échec de l'archivage de %s\n", argv[i]);}}
    string_delete(path);
  write(fd, zeros, BLOCKSIZE);
  write(fd, zeros, BLOCKSIZE);
  close(fd);
  return 0;
 error:
  if (errno) { perror("main"); }
  if (path) string_delete(path);
  exit(1);}
