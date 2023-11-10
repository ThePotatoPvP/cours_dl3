#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

char usage[] = " prend deux paramètres\n";

int main(int argc, char **argv) {
  int fd, nb;

  if (argc <= 2) {
    write(STDERR_FILENO, argv[0], strlen(argv[0]));
    write(STDERR_FILENO, usage, strlen(usage));
    return 1;
  }

  /* création et ouverture en écriture d'un nouveau fichier de nom
   * argv[2]; le programme doit terminer avec la valeur de retour 2 si
   * argv[2] existe, et avec la valeur de retour 1 dans le cas d'une
   * autre erreur.  */

  fd = open(argv[2], O_EXCL|O_CREAT|O_RDWR, S_IRWXU);
  if (fd <= -1) {
    if (errno == EEXIST){return 2;}
    else {return 1;}
  }


  /* écriture de la chaine argv[1] dans argv[2]; terminer avec la valeur
   * de retour 1 en cas d'erreur. */

  if (write(fd, argv[1], strlen(argv[1])) == -1){
    return 1;
  }

  /* fermeture du descripteur */

  close(fd);

  return 0;
}
