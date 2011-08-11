unit FunLabyEditConsts;

interface

resourcestring
  SErrorTitle = 'Erreur';
  SFatalErrorTitle = 'Erreur fatale';

  SBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalités coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  SDefaultPlayerName = 'Joueur';

  SConfirmExitTitle = 'Enregistrer le fichier';
  SConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

  SProjectAlreadyExists = 'Un projet avec ce nom de fichier existe déjà.';

  SCantSaveTitle = 'Impossible d''enregistrer';
  SCantSaveBadProjectFileName = 'Vous devez sélectionner un fichier '+
    '''<Mon Projet>.flp'' dans le dossier ''Projects\<Mon Projet>\';

  SAutoCompileSuccessTitle = 'Succès';
  SAutoCompileSuccess =
    'La recompilation de toutes unités a été faite avec succès.';

  SOnlyFieldOutsideTitle = 'Remplacer l''extérieur de la carte';
  SOnlyFieldOutside =
    'Seuls les terrains sont acceptés en dehors de la carte';

  SRemoveMapTitle = 'Supprimer une carte';
  SRemoveMap =
    'Êtes-vous certain de vouloir supprimer complètement cette carte ?';

  SRemoveSourceTitle = 'Retirer un fichier source';
  SRemoveSource =
    'Êtes-vous certain de vouloir retirer ce fichier source du projet ?';
  SErrorWhileOpeningSourceFile =
    'Une erreur est survenu lors de l''ouverture du fichier avec le message '+
    '"%s". Voulez-vous retirer ce fichier source du projet ?';

  SComponentDataTitle = 'Propriétés du composant';
  SPlayerDataTitle = 'Propriétés par joueur';
  SPlayerAttributesTitle = 'Attributs du joueur';
  SPlayerPluginsTitle = 'Plug-in attachés au joueur';

  SCollectionItemTitle = '%d - %s';

  SInvalidFileNameTitle = 'Nom de fichier invalide';
  SInvalidUnitName =
    '%s n''est pas un nom d''unité valide, il ne doit contenir que des '+
    'lettres non accentuées et des chiffres, et commencer par une lettre';

  SNoFieldTitle = 'Aucun terrain';
  SNoField =
    'Impossible de lancer cette opération car ce labyrinthe ne contient '+
    'aucun terrain';

  SResizeErrorTitle = 'Erreur de redimensionnement';
  SCantResizeToEmpty = 'Impossible de rétrécir plus la carte';

  SConfirmDeleteComponentTitle = 'Supprimer le composant';
  SConfirmDeleteComponent =
    'Êtes-vous sûr-e de vouloir supprimer ce composant ?';

implementation

end.

