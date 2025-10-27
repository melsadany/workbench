fixed_image=/Dedicated/jmichaelson-wdata/msmuhammad/refs/fsl-data/standard/MNI152_T1_0.5mm.nii.gz
moving_image=sub-2E_023-T1-to-MNI-SyN.nii.gz
output_prefix="overlay"          # Prefix for output PNGs
z_slices=(30 40 50 60)           # Slices to visualize


for z in "${z_slices[@]}"; do
    slicer $fixed_image $moving_image -s 2 -x 0.5 -y 0.5 -z $z \
        -o ${output_prefix}_z${z}.png
done


for z in "${z_slices[@]}"; do
    convert ${output_prefix}_z${z}.png \
        -fill '#ff0000' -colorize 30% \
        ${output_prefix}_z${z}_transparent.png
done


montage ${output_prefix}_z*_transparent.png \
    -tile 2x2 -geometry +2+2 montage.png


overlay_grid() {
    # Input arguments
    fixed_image=$1
    moving_image=$2
    output_prefix=${3:-"overlay_grid"}
    
    # Load image dimensions (assuming isotropic dimensions for simplicity)
    dims=($(fslinfo $fixed_image | awk '/dim[1-3]/{print $2}'))
    dim_x=${dims[0]}
    dim_y=${dims[1]}
    dim_z=${dims[2]}
    
    # Generate uniformly spaced slices
    slices_x=($(seq 0 $((dim_x / 8)) $((dim_x - 1))))
    slices_y=($(seq 0 $((dim_y / 8)) $((dim_y - 1))))
    slices_z=($(seq 0 $((dim_z / 8)) $((dim_z - 1))))

    # Temporary directory for intermediate PNGs
    temp_dir=$(mktemp -d)
    
    # Extract slices and save as PNGs
    for i in {0..8}; do
        slicer $fixed_image $moving_image -s 2 -x $(echo "scale=2; ${slices_x[i]}/${dim_x}" | bc) -o $temp_dir/x_slice_${i}.png
        slicer $fixed_image $moving_image -s 2 -y $(echo "scale=2; ${slices_y[i]}/${dim_y}" | bc) -o $temp_dir/y_slice_${i}.png
        slicer $fixed_image $moving_image -s 2 -z $(echo "scale=2; ${slices_z[i]}/${dim_z}" | bc) -o $temp_dir/z_slice_${i}.png
    done
    
    # Create a 9x9 montage (3 rows per orientation)
    montage $temp_dir/x_slice_*.png \
            $temp_dir/y_slice_*.png \
            $temp_dir/z_slice_*.png \
            -tile 9x3 -geometry +2+2 ${output_prefix}_grid.png

    # Clean up temporary files
    rm -r $temp_dir
    
    echo "Saved montage to ${output_prefix}_grid.png"
}

