#define _DEFAULT_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <limits.h>
#include <string.h>
#include <errno.h>

#include "mystring.h"

/*  Parcourt récursivement le répertoire de référence path pour y
 *  chercher les fichiers de nom (de base) target;
 *  retourne le nombre de fichiers trouvés (-1 en cas d'erreur) */
int process_dir(struct string * path, char * target_name) {
  int count = 0;
  DIR * dir = NULL;

  /* TODO[2] : parcourir les entrées du répertoire */
  DIR *dp = opendir(path->data);

  // Vérifie si l'ouverture du répertoire a réussi
  if (dp == NULL) {
    perror("Erreur lors de l'ouverture du répertoire");
    goto error;
  }

  struct dirent *entry;
  struct stat info;
  while ((entry = readdir(dp)) != NULL) {

  /* TODO[3] : pour chaque entrée, tester si son nom de base coïncide
   * avec celui cherché */
    if (strcmp(entry->d_name, target_name) == 0) {
      write(1, path->data, path->length);
      write(1, "/", 1);
      write(1, target_name, strlen(target_name));
      write(1, "\n", 1);
      count ++;
    }

    if (!(string_append(path, "/") && string_append(path, entry->d_name))){
      goto error;
    }

    if (lstat(path->data, &info) == 0 && S_ISDIR(info.st_mode) && (entry->d_name[0] != '.')){
      count += process_dir(path, target_name);
    }

    string_truncate(path, strlen(entry->d_name)+1);


  /* TODO[4] : si l'entrée considérée est un répertoire, y poursuivre
   * récursivement la recherche */
  }

  closedir(dir);
  return count;

 error:
  if (errno) { perror("process_dir"); }
  if (dir) closedir(dir);
  return -1;
}


int main(int argc, char ** argv) {
  struct string * path = NULL;
  char *target;
  int fd;

  path = string_new(PATH_MAX);
  /* Un buffer plus grand ne servirait à rien car on se sert de
   * open(path.data, ...) et stat(path.data, ...). */

  switch (argc) {
    case 2:
      if (!string_append(path, ".")) { goto error; }
      target = argv[1];
      break;
    case 3:
      if (!string_append(path, argv[1])) { goto error; }
      target = argv[2];
      break;
    default:
      dprintf(STDERR_FILENO, "usage: %s path target\n", argv[0]);
      goto error;
  }

  /* TODO[1] : vérifier que le chemin est valide et désigne un répertoire.
   * Terminer avec le code de retour 1 si ce n'est pas le cas */
  if ((fd = open(path->data, O_RDONLY)) < 0){
    return 1;
  }

  struct stat info;
  stat(path->data, &info);
  if (!(S_ISDIR(info.st_mode))){
    return 1;
  }

  /* TODO[1] : appeler process_dir(). */
  int p = process_dir(path, target);

  string_delete(path);
  path = NULL;

  /* TODO[5] Terminer avec un code de retour 0 si au moins un fichier a
   * été trouvé, et 1 sinon */

  if (p > 0){
    return 0;
  }

  return 1;

 error:
  if (errno) { perror("main"); }
  if (path) string_delete(path);
  return 1;
}
