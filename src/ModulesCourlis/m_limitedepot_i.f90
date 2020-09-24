Module M_LimiteDepot_I

Interface

Subroutine LimiteDepot (  &

  LimiteDepotG     ,  & ! numéro du point au delà duquel il peut y a voir dépôt ou érosion (1er pt immergé)
  LimiteDepotD     ,  & ! numéro du dernier pt pour lequel il peut y a voir dépôt ou érosion (dernier pt immergé)
  Zsurf         ,  & ! Cote de la surface libre
  ProfilCourlis      ,  & ! Profils sedimentaires
    NbProfils          ,  & ! Nombre de profils
  Erreur         )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction
!  --------
!    Calcul les indices des bornes RG et RD (LimitedepotG et LimiteDepotD)
!   de la partie immergée de chaque profil.
!  Il ne peut y avoir depot - erosion qu'entre ces points.
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer, intent(in)   :: NbProfils

  real(DOUBLE),           dimension(:), intent(in   ) :: Zsurf
  type(PROFIL_COURLIS_T), dimension(:), intent(in   ) :: ProfilCourlis

! Variables de sortie
  integer,                dimension(:), intent(  out) :: LimiteDepotG, LimiteDepotD

! Variables locale
!  integer :: i, j  ! PU2017 : Mis en commentaire

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel  ! PU2017 : Mis en commentaire

!=========================================================================

End Subroutine LimiteDepot

End Interface

End Module M_LimiteDepot_I
