#include <stdlib.h>
#include <unistd.h>

/* ./garfield lit l'entrée standard et la recopie sur la sortie standard,
 * caractère par caractère. Termine avec le code 1 si une erreur se
 * produit, et le code 0 sinon. */

int main() {
  char c;
  int nb;

  while(1) {
    /* TODO : lire un caractère sur stdin, *en utilisant l'appel système
     * read()*. En cas d'erreur de lecture, terminer sur une erreur (exit
     * code : 1). En cas de fin de fichier, sortir de la boucle. */

   nb = read(0, &c, 1);
   if (nb == -1){return 1;}
   if (nb == 0){break;}

    /* TODO : écrire sur stdout le caractère qui vient d'être lu, *en
     * utilisant l'appel système write()*. En cas d'erreur d'écriture,
     * terminer sur une erreur (exit code : 1) */

    if (write(1, &c, 1) == -1){return 1;}

  }
  /* TODO : Tout stdin a été correctement copié sur stdout : terminer avec
     l'exit code 0 */
}
