unit FunLabyEditConsts;

interface

resourcestring
  sFatalErrorTitle = 'Erreur fatale';

  sBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalités coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

  sDefaultPlayerName = 'Joueur';

  sPlayerPosition = 'Position : %s';
  sCenterToPosition = 'Centrer la vue sur ce joueur';
  sShowPlugins = 'Plug-in...';
  sShowAttributes = 'Attributs...';
  sShowObjects = 'Objets...';

  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

  sCantSave = 'Impossible d''enregistrer';
  sCantSaveUnplacedPlayer =
    'Impossible d''enregistrer car le joueur d''ID %s n''a pas été placé';

  sCantCenterToPosition = 'Impossible de centrer le joueur';
  sCantCenterToUnplacedPlayer =
    'Impossible de centrer sur ce joueur car il n''a pas été placé';

  sReplaceOutsideTitle = 'Remplacer l''extérieur de la carte';
  sReplaceOutside = 'Voulez-vous vraiment modifier l''extérieur de la carte ?';

  sRemoveMapTitle = 'Retirer une carte';
  sRemoveMap = 'Êtes-vous certain de vouloir retirer cette carte du projet ?';

  sRemoveSourceTitle = 'Retirer un fichier source';
  sRemoveSource =
    'Êtes-vous certain de vouloir retirer ce fichier source du projet ?';

  sNoUnitOpenerHandlerTitle = 'Pas de gestionnaire disponible';
  sNoUnitOpenerHandler = 'Aucun gestionnaire disponible pour l''ajout d''unité';

  sNoUnitCreatorTitle = 'Pas de gestionnaire disponible';
  sNoUnitCreator = 'Aucun gestionnaire disponible pour la création d''unité';

  sDuplicateUnitTitle = 'Unité déjà présente';
  sDuplicateUnit = 'Il y a déjà une unité ''%s''';

  sRemoveUnitTitle = 'Supprimer une unité';
  sCantRemoveLastUnit = 'Il faut toujours garder au moins une unité';
  sConfirmRemoveUnit = 'Êtes-vous sûr-e de vouloir supprimer l''unité ''%s'' ?';

implementation

end.

