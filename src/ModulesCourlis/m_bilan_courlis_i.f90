Module M_Bilan_Courlis_I

Interface

Subroutine Bilan_Courlis(  &

  FluxVase            ,  & ! Bilan sur les flux de vases
  FluxSable           ,  & ! Bilan sur les flux de sables
  MasseVase           ,  & ! Bilan sur les masses de vases
  MasseSable          ,  & ! Bilan sur les masses de sables
  DepotCumulCouche  ,  & ! Depot cumule /profil et /couche (> 0 dépôt, < 0 érosion)
  VolSedDepot         ,  & ! Volume de sédimt déposé depuis début du calcul
  DeltaSurfaceSed    ,  & ! Variation de la surface sédimentaire
  CVase        ,  & ! Concentration des vases en suspension
  CSable        ,  & ! Concentration des sables en suspension
  QVaseCouche      ,  & ! Flux de dépôt des vases (> 0 dépôt, < 0 érosion)
  QSableCouche    ,  & ! Flux de dépôt des sables (> 0 dépôt, < 0 érosion)
  QVase        ,  & ! Flux de dépôt des vases par couche (> 0 dépôt, < 0 érosion)
  QSable        ,  & ! Flux de dépôt des sables par couche (> 0 dépôt, < 0 érosion)
  QApportVase      ,  & ! Flux de d'apport lineaires des vases
  QApportSable    ,  & ! Flux de d'apport lineaires des sables
!  DeltaH        ,  & ! Variation de hauteur sédimentaire en chaque point des profils
  Dt          ,  & ! Pas de temps
  ProfilCourlis    ,  & ! Profils sedimentaires
  Absc        ,  & ! Abscisse des sections de calcul (ProfilCourlis%Abs)
  Zsurf        ,  & ! Cote de la surface libre
  Vit          ,  & ! Vitesse moyenne par section
    Sm          ,  & ! Surface mouillee
  CoucheSed      ,  & ! Paramètres sédimentaires des différentes couches
  Erreur        )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcul du bilan sédimentaire en masse
!  --------
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info

use M_PROFIL_COURLIS_T    ! Définition du type PROFIL_COURLIS_T
use M_COUCHE_T        ! Définition du type COUCHE_T
use M_BILAN_FLUX_T      ! Definition du type BILAN_FLUX_T
use M_BILAN_MASSE_T      ! Definition du type BILAN_MASS_T

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE), intent(in) :: Dt

  real(DOUBLE)      , dimension(:)  , intent(in) :: Zsurf, Vit, Sm
  real(DOUBLE)      , dimension(:)  , intent(in) :: CVase, CSable
  real(DOUBLE)      , dimension(:,:), intent(in) :: QVaseCouche, QSableCouche
  real(DOUBLE)      , dimension(:)  , intent(in) :: QVase, QSable
  real(DOUBLE)      , dimension(:)  , intent(in) :: QApportVase, QApportSable
!  real(DOUBLE)      , dimension(:,:), intent(in) :: DeltaH
  type(COUCHE_T)    , dimension(:)  , intent(in) :: CoucheSed
  type(PROFIL_COURLIS_T), dimension(:)  , intent(in) :: ProfilCourlis
  real(DOUBLE)      , dimension(:)  , intent(in) :: Absc

! Variables de sortie
  type(BILAN_FLUX_T)         , intent(  out) :: FluxVase, FluxSable
  type(BILAN_MASSE_T)         , intent(inout) :: MasseVase, MasseSable
  real(DOUBLE)     , dimension(:)   , intent(inout) :: DeltaSurfaceSed
  real(DOUBLE)     , dimension(:,:), intent(inout) :: DepotCumulCouche
  real(DOUBLE)             , intent(inout) :: VolSedDepot

! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer :: i, j, k    ! Compteurs
!  integer :: NbProfil    ! Nombre de sections de calcul
!  integer :: NbCouche    ! Nombre de couches sédimentaires
!  integer :: NPt
!  real(DOUBLE) :: Err

!  real(DOUBLE) :: MasseVaseEauPrec, MasseSableEauPrec ! Masse de sédiment présente dans l'eau au pas de tps précédent

!  real(DOUBLE)  , dimension(:), allocatable :: FluxVaseCouche, FluxSableCouche

! Traitement des erreurs
! PU2017 : Mise en commentaire des var loc pour traitement erreur
  type(ERREUR_T), intent(inout) :: Erreur
!  integer            :: retour      ! Code de retour de la fonction read, allocate
!  character(132)        :: arbredappel_old  ! Ancien arbre d'appel

!=========================================================================

End Subroutine Bilan_Courlis

End Interface

End Module M_Bilan_Courlis_I
