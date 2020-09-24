Module M_StabiliteTalus_I

Interface

Subroutine StabiliteTalus  (  &

  ProfilCourlis      ,  & ! Profils sedimentaires
  Talus          ,  & ! Paramètres relatifs aux talus
    Dt            ,  & ! Pas de temps
  Zsurf          ,  & ! Cote de la surface libre
  CoucheSed        ,  & ! Paramètres sédimentaires des différentes couches
  DeltaH          ,  & ! Variation de hauteur sédimentaire en chaque point des profils
  Resini          ,  & ! Resistance initiale des blocs au mouvement
  SurPl          ,  & !
  Erreur          )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Etablie la stabilite des berges et calcule la forme du lit
!  --------    apres glissement via une matrice de diffusion
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

use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_COUCHE_T        ! Définition du type COUCHE_T
use M_TALUS_T        ! Définition du type TALUS_T

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)        :: Dt  ! Temps,  ! PU2017 : Mise en commentaire de Temps

  real(DOUBLE)      , dimension(:)  , intent(in   )  :: Zsurf
  type(COUCHE_T)    , dimension(:)  , intent(in   )  :: CoucheSed
  type(TALUS_T)              , intent(in   )  :: Talus
  real(DOUBLE)      , dimension(:,:), intent(in   )  :: Resini

! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout)  :: ProfilCourlis
  real(DOUBLE)      , dimension(:,:), intent(inout)  :: DeltaH
  integer        , dimension(:,:), intent(inout)  :: SurPl

! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer :: i, j, k        ! Compteurs
!  integer :: j1, jg, jd, jj, kk    ! Compteurs
!  integer :: NbProfil        ! Nombre de profils
!  integer :: NInt          ! Nombre d'interfaces sédimentaires
!  integer :: NPt          ! Nombre de points décrivant le profil en travers

!  real(DOUBLE) :: ImG, ImD      ! Variable a 1 si le point gauche (resp. droit) est dans l'eau, 0 sinon
!  real(DOUBLE) :: HeG, HeD, HeM    ! Hauteurs de la colonne d'eau au dessus du bloc
!  real(DOUBLE) :: HsG, HsD, HsM    ! Hauteurs de la colonne sédimentaire (gauche, droite, moyenne)
!  real(DOUBLE) :: HsGC, HsDC, HsMC  ! Hauteurs de la couche en cours
!  real(DOUBLE) :: HsC        ! Hauteur de sediment dans la couche traitee
!  real(DOUBLE) :: Dx, DxG, DxD    ! Pas de discretisation horizontaux
!  real(DOUBLE) :: DzS, DxzS      ! Pas de discretisation vertical, hypothenuse a la surface du bloc
!  real(DOUBLE) :: DzF, DxzF     ! Pas de discretisation vertical, hypothenuse du fond du bloc
!  real(DOUBLE) :: SinS, CosS, SinF, CosF  ! Angles du bloc
!  real(DOUBLE) :: Pente        ! pente de la surface du bloc
!  real(DOUBLE) :: P, EG, ED, W, Mot  ! forces motrices exercees sur le bloc
!  real(DOUBLE) :: Res        ! forces de resistance au mouvement
!  real(DOUBLE) :: Sec        ! facteur de securite
!  real(DOUBLE) :: CG, CD      ! facteur de la matrice "lumpee"
!  real(DOUBLE) :: Ch        ! facteur reducteur de Lam

!  real(DOUBLE) :: z_ref        ! point bas des interfaces sedimentaires

!  real(DOUBLE), dimension(:,:), allocatable :: Z      ! Cotes des interfaces sédimentaires
!  real(DOUBLE), dimension(:,:), allocatable :: Stab      ! Vecteur de stabilite des elements
!  real(DOUBLE), dimension(:)  , allocatable :: A, C      ! Vecteur de la matrice
!  real(DOUBLE), dimension(:)  , allocatable :: Lam      ! Parametre pour adapter la vitesse du glissement
!  real(DOUBLE), dimension(:)  , allocatable :: Dz      ! Vecteur de correction de la cote des interfaces
!  real(DOUBLE), dimension(:)  , allocatable :: DzPart    ! Deformation des couches sedimentaires

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  integer      :: retour  ! Code de retour de la fonction read, allocate ! PU2017 : Mis en commentaire
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel  ! PU2017 : Mis en commentaire

! Constantes
! PU2017 : Mise en commentaire des constantes
!  real(DOUBLE), parameter :: HsMin  = 5 * EPS3  ! Hauteur de couche minimale
!  real(DOUBLE), parameter :: EpsH   = EPS8    !

!=========================================================================

End Subroutine StabiliteTalus

End Interface

End Module M_StabiliteTalus_I
