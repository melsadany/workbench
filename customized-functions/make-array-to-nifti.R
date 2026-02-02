make_nifti_from_df <- function(df, ref="/Dedicated/jmichaelson-wdata/msmuhammad/refs/fsl-data/standard/MNI152_T1_2mm_brain.nii.gz") {
  library(oro.nifti)
  # 1. Read template image
  template_nifti <- readNIfTI(ref, reorient = FALSE)
  # 2. Ensure float storage
  template_nifti@"datatype" <- 16  # FLOAT32
  template_nifti@"bitpix" <- 32
  # 3. Zero out all intensities
  template_nifti[] <- 0
  # 4. Bounds check (optional but recommended)
  dims <- dim(template_nifti)
  df <- subset(df, x >= 1 & x <= dims[1] &
                 y >= 1 & y <= dims[2] &
                 z >= 1 & z <= dims[3])
  # 5. Convert 3D indices to linear indices
  linear_map <- function(x, y, z) {
    return((z - 1) * dims[1] * dims[2] + (y - 1) * dims[1] + x)
  }
  lin_idx <- with(df, linear_map(x, y, z))
  # 6. Assign intensity values directly
  img_vector <- as.vector(template_nifti)
  img_vector[lin_idx] <- df$intensity
  template_nifti[] <- img_vector
  # 6. Return result
  return(template_nifti)
}

################################################################################
################################################################################
################################################################################
## V2

library(RNifti)

# df: columns x, y, z (1-based voxel indices), intensity (or any value)
# template_path: path to an existing NIfTI in the desired MNI grid
make_nifti_from_df_V2 <- function(df, 
                                  template_path= "/Dedicated/jmichaelson-wdata/msmuhammad/refs/fsl-data/standard/MNI152_T1_2mm_brain.nii.gz", 
                                  out_path, value_col = "intensity") {
  # 1. Read template (provides dim, affine, header) [web:2]
  template <- readNifti(template_path)
  dims <- dim(template)
  
  if (length(dims) < 3L) {
    stop("Template must be at least 3D.")
  }
  
  # 2. Create empty array (3D) matching template
  arr <- array(0, dim = dims[1:3])
  
  # 3. Sanity check voxel indices
  stopifnot(all(df$x >= 1 & df$x <= dims[1]),
            all(df$y >= 1 & df$y <= dims[2]),
            all(df$z >= 1 & df$z <= dims[3]))
  
  # 4. Fill array at specified voxels
  # convert (x,y,z) to linear index in R [x + (y-1)*nx + (z-1)*nx*ny]
  nx <- dims[1]; ny <- dims[2]
  lin_idx <- df$x + (df$y - 1L) * nx + (df$z - 1L) * nx * ny
  
  vals <- df[[value_col]]
  arr[lin_idx] <- vals
  
  # 5. Turn array into niftiImage using template metadata [web:62]
  img <- asNifti(arr, reference = template)  # copies header/affine from template
  
  # 6. Write to disk [web:70]
  writeNifti(img, out_path)
  
  invisible(out_path)
}
