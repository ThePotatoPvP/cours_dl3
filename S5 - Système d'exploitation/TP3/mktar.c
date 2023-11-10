#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <libgen.h>
#include <sys/param.h>
#include "tarutils.h"

char zeros[BLOCKSIZE] = {0};
char buf[BLOCKSIZE];
struct posix_header hd;


/* ajoute un fichier de nom filename à l'archive ouverte (fd)
 * retourne 0 en cas de succès, 1 en cas d'échec */
int tar_one_file(int fd, char * filename) {
  struct stat st;
  struct posix_header * phd = &hd;
  int file_fd;

  /* TODO : création de l'entête; commencer par le remplir de 0, puis
   * renseigner au moins les champs indispensables : typeflag, name,
   * mode, size, et _en dernier_ checksum.
   * Pour typeflag, mode et size, utiliser stat() */
  memset(phd, 0, BLOCKSIZE);
  strncpy(phd->name, filename, sizeof(phd->name));

  if (stat(filename, &st) <0){
    return 1;
  }

  phd->typeflag = '0';
  snprintf(phd->mode, 8, "%07o", st.st_mode);
  snprintf(phd->size, 12, "%011lo", st.st_size);


  /* calcul de checksum : indispensable pour gnu tar :-( */
  set_checksum(phd);

  /* TODO : ouverture du fichier à copier */
  if ((file_fd = open(filename, O_RDONLY)) < 0) {
    perror("open");
    return 1;
  }

  /* TODO : copie de l'entête dans l'archive */
  write(fd, phd, BLOCKSIZE);

  /* TODO : copie du fichier; ATTENTION à compléter le dernier bloc si
   * nécessaire */
  off_t file_size = st.st_size;
  while (file_size > 0) {
    ssize_t n = read(file_fd, buf, BLOCKSIZE);
    if (n < 0) {
      perror("read");
      close(file_fd);
      return 1;
    }

    if (n == 0) {
      /* Pad the last block with zeros if necessary */
      write(fd, zeros, BLOCKSIZE);
      break;
    }

    write(fd, buf, n);
    file_size -= n;
  }

  /* TODO : fermeture du fichier copié */
  close(file_fd);

  return 0;
}



int main(int argc, char **argv){
  if (argc < 3) {
    fprintf(stderr, "Usage : %s file.tar f1 [... fn]\n", argv[0]);
    exit(1);
  }

  /* TODO : création du fichier d'archive */
  int fd;
  if ((fd = open(argv[1], O_RDONLY | O_CREAT | O_EXCL)) <0){
    exit(1);
  }

  /* TODO : boucle principale pour ajouter chaque argv[i] l'un après
   * l'autre */
  for (int i=2; i< argc; i++){
    if (tar_one_file(fd, argv[i]) == 1){
      exit(1);
    }
  }

  /* TODO : finalisation : ajout de deux blocs vides */
  for (int i=0; i<2; i++){
    write(fd, zeros, BLOCKSIZE);
  }

  /* TODO : fermeture du fichier tar */
  close(fd);

  exit(0);
}
