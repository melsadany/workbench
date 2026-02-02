import numpy as np
import numpy as np
import nibabel as nib
from nilearn import plotting, datasets
from nilearn.plotting import plot_glass_brain, show


stat_img='/wdata/msmuhammad/projects/RPOE/mri/data/derivatives/func/SPM/lang-iq-pcs/task-resids/spm_results/p_uncorr_0001.nii'
stat_img='/wdata/msmuhammad/projects/RPOE/mri/data/derivatives/func/SPM/lang-iq-pcs/task-resids/spm_results/spmT_0003.nii'

# option 1
plot_glass_brain(stat_img,plot_abs=True,threshold=4)
plotting.show()

# option 2
display = plot_glass_brain(stat_img, threshold=2,plot_abs=False, display_mode="lzry",colorbar=True)
plotting.show()

display = plot_glass_brain(None, plot_abs=False, display_mode="lzry")
display.add_contours(stat_img, levels=[4,np.inf], colors="r", linewidths=2.0,filled=True,alpha=0.7)
display.add_contours(stat_img, levels=[-np.inf,-4], colors="b", linewidths=2.0,filled=True,alpha=0.7)
plotting.show()


# color change
# hex_colors=['#ff4600','#4782b4']
# rgb_colors = [tuple(int(h.lstrip('#')[i:i+2], 16)/255 for i in (0, 2, 4)) for h in hex_colors]
# display.add_contours(
#     stat_img, 
#     levels=[3], 
#     colors=rgb_colors, filled=True
#     )
# plotting.show()

