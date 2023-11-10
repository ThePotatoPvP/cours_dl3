#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include "tarutils.h"

#define MIN(a,b) (((a)<(b))?(a):(b))

char buf[BLOCKSIZE];
struct posix_header hd;

/* extraie de l'archive dont fd est un descripteur le fichier décrit par
 * l'entête phd : crée un fichier de nom phd -> name
 * et de contenu constitué des filesize octets suivants de l'archive.
 * Attention à respecter les droits phd -> mode */
int extract(int fd, struct posix_header * phd, unsigned int filesize) {

  /* TODO : vérification du type de fichier : seuls les REGTYPE et AREGTYPE
   * sont supportés */
  if (phd->typeflag != REGTYPE && phd->typeflag != AREGTYPE) {
    fprintf(stderr, "Type de fichier non pris en charge %s\n", phd->name);
    return -1;
  }

  /* TODO : création du fichier avec les bons droits */
  int out_fd;
  if ((out_fd = open(phd->name, O_WRONLY | O_CREAT, phd->mode)) < 0) {
    perror("Erreur lors de l'ouverture du fichier de sortie");
    return 1;
  }

  /* TODO : recopie du contenu; ATTENTION au dernier bloc! */
  int bytes_written = 0;
  while (bytes_written < filesize) {
    int to_read = MIN(BLOCKSIZE, filesize - bytes_written);
    int bytes_read = read(fd, buf, to_read);

    if (bytes_read <= 0) {
      perror("Erreur lors de la lecture de l'archive");
      close(out_fd);
      return 1;
    }

    int written_now = write(out_fd, buf, bytes_read);

    if (written_now != bytes_read) {
      perror("Erreur lors de l'écriture du fichier de sortie");
      close(out_fd);
      return 1;
    }

    bytes_written += written_now;
  }
  /* TODO : refermer le descripteur */

  close(out_fd);
  return 0;
}


int main(int argc, char **argv){
  int ret = 0, all;
  char * filename = NULL;
  struct posix_header * phd = &hd;

  switch (argc) {
    case 2 : all = 1; break;
    case 3 : all = 0; filename = argv[2]; break;
    default :
      fprintf(stderr, "Usage : %s file.tar [file]\n", argv[0]);
      exit(1);
  }

  /* TODO : ouverture du fichier tar */
  int fd = open(argv[1], O_RDONLY);
  if (fd < 0) {
    perror("Erreur lors de l'ouveture du fichier tar");
    exit(1);
  }

  int fd2;
  /* boucle principale */
  while (1) {
    /* TODO : lecture de l'entête; vérifier que la lecture est bien complète */
    int bytes_read = read(fd, phd, BLOCKSIZE);
    if (bytes_read < BLOCKSIZE) {
      if (bytes_read == 0){break;}
      else {
        fprintf(stderr, "Erreur lors de la lecture de l'entête.\n");
        ret = 1;
        break;
      }
    }

    /* TODO : la fin de l'archive est atteinte si le bloc est rempli de zéros,
     * et en particulier si name == "" */
    if (strlen(hd.name) == 0) {
      break;
    }

    /* vérification de checksum (facultatif) */
    if (!check_checksum(phd)) {
      fprintf(stderr, "Checksum erronée : %s\n", phd -> chksum);
      ret = 1;
      break;
    }

    /* TODO : la taille et le nombre de blocs sont toujours utiles */
    unsigned int filesize = 0;
    sscanf(phd->size, "%o", &filesize);

    /* TODO[1] s'il s'agit du fichier cherché, le désarchiver puis
     * quitter; sinon, sauter à l'entête suivant */
    if ( all || strcmp(filename, hd.name) == 0){
      fd2 = open(hd.name, O_RDONLY);
      if (fd2 > 0){
        if(extract(fd2, phd, filesize)){
          exit(1);
        } else {
          break;
        }
      } else {
        exit(1);
      }
    }
  }

  close(fd);
  exit(ret);
}
