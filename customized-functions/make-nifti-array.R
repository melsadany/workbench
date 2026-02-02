library(oro.nifti)
library(tidyverse)
make_img_array <- function(file, dim = 3, labels = NULL) {
  nifti_file <- readNIfTI(file, reorient = FALSE)
  # Extract image data as an array
  img_data <- as.array(nifti_file)
  # Get voxel dimensions and origin (qform)
  voxel_dim <- nifti_file@pixdim[2:4]
  origin <- c(nifti_file@qoffset_x,nifti_file@qoffset_y, nifti_file@qoffset_z)
  
  if (dim==3) {
    # Create a grid of voxel indices
    xyz_grid <- expand.grid(
      x = 1:dim(img_data)[1],
      y = 1:dim(img_data)[2],
      z = 1:dim(img_data)[3]
    )
    # Extract voxel intensities
    xyz_grid$intensity <- as.vector(img_data)
    # Filter out zero-intensity voxels if needed
    xyz_grid <- xyz_grid %>% filter(intensity != 0)
    # Calculate MNI coordinates using voxel dimensions and origin
    xyz_grid <- xyz_grid %>%
      mutate(
        mni_x = (x - 1) * voxel_dim[1] - origin[1],
        mni_y = (y - 1) * voxel_dim[2] + origin[2],
        mni_z = (z - 1) * voxel_dim[3] + origin[3]
      ) %>%
      select(x,y,z, mni_x, mni_y, mni_z, intensity)
    if (is.null(labels)) {
      xyz_wide <- xyz_grid
    }else {
      voxel.labels <- read_rds(labels)
      xyz_wide <- inner_join(voxel.labels, xyz_grid)
    }
    return(xyz_wide)
  } else if(dim==4) {
    xyzzy_grid <- expand.grid(
      x = 1:dim(img_data)[1],
      y = 1:dim(img_data)[2],
      z = 1:dim(img_data)[3],
      t = 1:dim(img_data)[4]
    )
    # Extract intensity values per frame
    xyzzy_grid$intensity <- as.vector(img_data)
    xyzzy_grid <- xyzzy_grid %>%
      mutate(
        mni_x = (x - 1) * voxel_dim[1] - origin[1],
        mni_y = (y - 1) * voxel_dim[2] + origin[2],
        mni_z = (z - 1) * voxel_dim[3] + origin[3]
      ) %>%
      select(x,y,z, mni_x, mni_y, mni_z, t, intensity)
    gc()
    voxel.labels <- read_rds(labels)
    xyzzy_wide <- inner_join(voxel.labels, xyzzy_grid) %>%
      pivot_wider(
        id_cols = c(colnames(voxel.labels)),
        names_from = t,
        values_from = intensity,
        names_prefix = "Frame_"
      )
    return(xyzzy_wide)
  }
}
################################################################################
################################################################################
################################################################################
## version 2 of the function
library(RNifti)

make_img_array_V2 <- function(nifti_path,
                              drop_zero = TRUE,
                              drop_na   = TRUE) {
  img <- readNifti(nifti_path)  # works for 3D or 4D [web:35]
  dim_img <- dim(img)
  
  if (length(dim_img) < 3L || length(dim_img) > 4L) {
    stop("Only 3D or 4D NIfTI images are supported.")
  }
  
  nx <- dim_img[1]
  ny <- dim_img[2]
  nz <- dim_img[3]
  nt <- if (length(dim_img) == 4L) dim_img[4] else 1L
  
  # Voxel indices (1-based, as RNifti expects) [web:11]
  idx3 <- as.matrix(expand.grid(
    x = seq_len(nx),
    y = seq_len(ny),
    z = seq_len(nz)
  ))
  
  # MNI coords for spatial voxels only (independent of time)
  mni_coords <- voxelToWorld(idx3, img)  # uses image xform [web:11]
  
  # Repeat for each time/volume
  idx_list <- vector("list", nt)
  val_list <- vector("list", nt)
  
  for (t in seq_len(nt)) {
    if (nt == 1L) {
      vol <- img
    } else {
      vol <- img[,,, t, drop = FALSE]  # 3D slice of 4D image [web:15]
    }
    
    vals <- as.vector(vol)
    
    keep <- rep(TRUE, length(vals))
    if (drop_zero) keep <- keep & (vals != 0)
    if (drop_na)   keep <- keep & !is.na(vals)
    
    # Subset 3D indices and MNI coords with same mask
    idx_keep <- idx3[keep, , drop = FALSE]
    mni_keep <- mni_coords[keep, , drop = FALSE]
    
    idx_list[[t]] <- data.frame(
      x      = idx_keep[, 1],
      y      = idx_keep[, 2],
      z      = idx_keep[, 3],
      mni_x  = mni_keep[, 1],
      mni_y  = mni_keep[, 2],
      mni_z  = mni_keep[, 3],
      t_vol  = t,
      intensity = vals[keep]
    )
  }
  
  df <- do.call(rbind, idx_list)
  rownames(df) <- NULL
  df
}



