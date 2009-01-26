unit FunLabyEditConsts;

interface

resourcestring
  sFatalErrorTitle = 'Erreur fatale';

  sBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalit�s coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore impl�ment�e';

  sDefaultPlayerName = 'Joueur';

  sPlayerPosition = 'Position : %s';
  sCenterToPosition = 'Centrer la vue sur ce joueur';
  sShowPlugins = 'Plug-in...';
  sShowAttributes = 'Attributs...';
  sShowObjects = 'Objets...';

  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a �t� modifi�. Voulez-vous l''enregistrer ?';

  sCantSave = 'Impossible d''enregistrer';
  sCantSaveUnplacedPlayer =
    'Impossible d''enregistrer car le joueur d''ID %s n''a pas �t� plac�';

  sCantCenterToPosition = 'Impossible de centrer le joueur';
  sCantCenterToUnplacedPlayer =
    'Impossible de centrer sur ce joueur car il n''a pas �t� plac�';

  sReplaceOutsideTitle = 'Remplacer l''ext�rieur de la carte';
  sReplaceOutside = 'Voulez-vous vraiment modifier l''ext�rieur de la carte ?';

  sRemoveMapTitle = 'Retirer une carte';
  sRemoveMap = '�tes-vous certain de vouloir retirer cette carte du projet ?';

  sRemoveSourceTitle = 'Retirer un fichier source';
  sRemoveSource =
    '�tes-vous certain de vouloir retirer ce fichier source du projet ?';

  sNoUnitOpenerHandlerTitle = 'Pas de gestionnaire disponible';
  sNoUnitOpenerHandler = 'Aucun gestionnaire disponible pour l''ajout d''unit�';

  sNoUnitCreatorTitle = 'Pas de gestionnaire disponible';
  sNoUnitCreator = 'Aucun gestionnaire disponible pour la cr�ation d''unit�';

  sDuplicateUnitTitle = 'Unit� d�j� pr�sente';
  sDuplicateUnit = 'Il y a d�j� une unit� ''%s''';

  sRemoveUnitTitle = 'Supprimer une unit�';
  sCantRemoveLastUnit = 'Il faut toujours garder au moins une unit�';
  sConfirmRemoveUnit = '�tes-vous s�r-e de vouloir supprimer l''unit� ''%s'' ?';

implementation

end.

