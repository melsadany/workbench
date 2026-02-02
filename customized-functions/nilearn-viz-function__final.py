#!/usr/bin/env python

import argparse
import os
import nibabel as nib
import numpy as np
import matplotlib.pyplot as plt
from nilearn import surface, plotting
from nilearn.datasets import load_fsaverage, load_fsaverage_data
from nilearn.image import load_img
from nilearn.plotting import plot_glass_brain, plot_img_on_surf, plot_surf_stat_map


def parse_args():
    parser = argparse.ArgumentParser(
        description="Nilearn visualization of t-stat and p-value maps.")
    parser.add_argument("--t_img", required=True, help="Path to t-stat image (NIfTI).")
    parser.add_argument("--p_img", required=True, help="Path to p-value image (NIfTI).")
    parser.add_argument("--p_thresh", type=float, required=True,
        help="P-value threshold (e.g. 0.001).")
    parser.add_argument("--plot_mode", choices=["masked", "full"], default="masked",
        help="Use p-mask ('masked') or full stat map ('full').")
    parser.add_argument("--t_thresh", type=float, default=None,
        help="Optional t-stat threshold (e.g. 2.0). If None, no extra t-threshold.")
    # parser.add_argument("--surf_hemi", choices=["left", "right", "both"], default="left",
    #   help="Hemisphere for plot_surf_stat_map.")
    # parser.add_argument("--surf_view",default="lateral",
    #   help="View for plot_surf_stat_map (e.g. lateral, medial, dorsal, ventral, anterior, posterior).")
    parser.add_argument("--title", default="Statistical map",
        help="Figure title.")
    parser.add_argument("--outdir", required=True,
        help="Output directory to save PNG figures.")
    parser.add_argument("--black_bg", action="store_true",
        help="If set, use black background; otherwise white.")
    parser.add_argument("--stat_label", default="stat",
        help="Short label for the stat image to embed in filenames (e.g. langPC1).")
    parser.add_argument("--dpi", type=int, default=300,
        help="DPI for saved PNGs (e.g. 300–360).")
    return parser.parse_args()

def save_all_surf_views(surf_img, fsavg, fsavg_sulcal, stat_title, out_png, t_thresh=None,
    black_bg=True, cmap="cold_hot",):
    hemis = ["left", "right"]
    views = ["lateral", "medial", "dorsal", "ventral", "anterior", "posterior"]

    n_rows = len(hemis)
    n_cols = len(views)

    facecolor = "black" if black_bg else "white"
    text_color = "white" if black_bg else "black"

    fig, axes = plt.subplots(n_rows, n_cols, subplot_kw={"projection": "3d"},
        figsize=(3 * n_cols, 3 * n_rows), facecolor=facecolor,
    )
    # fsavg_sulcal is a SurfaceImage; sulcal arrays per hemisphere:
    sulc_left = fsavg_sulcal.data.parts["left"]
    sulc_right = fsavg_sulcal.data.parts["right"]  # PolyData-like, but accepted as bg_map[web:50]
    
    cbar_mappable = None
    cbar_ax = None

    for i, hemi in enumerate(hemis):
        for j, view in enumerate(views):
            ax = axes[i, j]
            if hemi == "left":
                surf_mesh = fsavg["inflated"].parts["left"]  # or fsavg["pial"].parts["left"][web:62][web:73]
                bg_map = sulc_left
            else:
                surf_mesh = fsavg["inflated"].parts["right"]
                bg_map = sulc_right
            # Only first panel gets a colorbar; others don't
            want_cbar = (i == 0 and j == 0)
            
            img = plot_surf_stat_map(stat_map=surf_img, surf_mesh=surf_mesh,
                hemi=hemi, view=view, 
                threshold=t_thresh if t_thresh is not None else 2.0,
                bg_map=bg_map, bg_on_data=True, cmap=cmap,
                colorbar=False, figure=fig, axes=ax,
            )
            # Small label instead of big title box
            ax.set_title(f"{hemi[0].upper()}-{view}", color=text_color, backgroundcolor="none", fontsize=8)
            if want_cbar:
                # Matplotlib puts the colorbar in a new axes attached to this fig
                # Find it and keep its ScalarMappable
                for a in fig.axes:
                    if a is not ax and a not in axes.ravel():
                        cbar_ax = a
                        break
                if cbar_ax is not None:
                    cbar_mappable = cbar_ax.images[0] if cbar_ax.images else None

    fig.suptitle(stat_title, color=text_color, fontsize=14)

    # If we found a colorbar axis, move it to a single shared position
    if cbar_ax is not None and cbar_mappable is not None:
        # Remove the original colorbar
        cbar_ax.remove()
        # Create a new axis for the shared colorbar
        shared_cax = fig.add_axes([0.92, 0.15, 0.02, 0.7])
        cb = fig.colorbar(cbar_mappable, cax=shared_cax)
        cb.ax.tick_params(colors=text_color)
        for spine in cb.ax.spines.values():
            spine.set_color(text_color)

        fig.tight_layout(rect=[0, 0, 0.9, 1])
    else:
        fig.tight_layout()

    fig.savefig(out_png, dpi=300, facecolor=facecolor, edgecolor=facecolor)
    plt.close(fig)



def main():
    args = parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    # figure-name suffix describing parameters
    if args.plot_mode == "masked":
        map_part = f"pMASK_lt{args.p_thresh:g}"
    else:
        map_part = "pFULL"  # explicitly indicates no p-thresholding
    # T-stat part
    if args.t_thresh is not None:
        t_part = f"tGE_{args.t_thresh:g}"
    else:
        t_part = "tALL"
    # Stat label part
    stat_part = f"stat_{args.stat_label}"
    suffix = f"{map_part}__{t_part}__{stat_part}"
    
    # Load data
    fsavg = load_fsaverage(mesh="fsaverage5")
    fsavg_sulc = load_fsaverage_data(mesh="fsaverage5",mesh_type="inflated",data_type="sulcal")
    stat_img = load_img(args.t_img)
    t_img = nib.load(args.t_img)
    p_img = nib.load(args.p_img)

    # Build p-value mask if needed
    if args.plot_mode == "masked":
        t_data = t_img.get_fdata()
        p_data = p_img.get_fdata()
        mask = p_data < args.p_thresh
        if args.t_thresh is not None:
            mask &= (t_data >= args.t_thresh)
        t_masked_data = np.zeros_like(t_data)
        t_masked_data[mask] = t_data[mask]
        stat_in = nib.Nifti1Image(t_masked_data, affine=t_img.affine, header=t_img.header)
    else:
        # optional extra t-threshold on full map
        if args.t_thresh is not None:
            t_data = t_img.get_fdata()
            mask = t_data >= args.t_thresh
            t_thr_data = np.zeros_like(t_data)
            t_thr_data[mask] = t_data[mask]
            stat_in = nib.Nifti1Image(t_thr_data, affine=t_img.affine, header=t_img.header)
        else:
            stat_in = stat_img

    # Surface projection 
    surf_img = surface.SurfaceImage.from_volume(mesh=fsavg["pial"], volume_img=stat_in)
    # (inflated, left hemi)
    # fig_surf = plot_surf_stat_map(stat_map=surf_img, surf_mesh=fsavg,
    #    hemi=args.surf_hemi, view=args.surf_view, colorbar=True,
    #    threshold=args.t_thresh if args.t_thresh is not None else 2.0,
    #    bg_map=None, cmap="cold_hot", title=args.title,
    # ) # returns Figure for matplotlib engine[web:42][web:45]
    # surf_png = os.path.join(args.outdir, f"surf_stat_left_{suffix}.png")
    # fig_surf.savefig(surf_png, dpi=args.dpi)
    # plt.close(fig_surf)
    # Surface figure background
    # if args.black_bg:
    #    fig_surf.patch.set_facecolor("black")
    # else:
    #    fig_surf.patch.set_facecolor("white")

    # all
    surf_all_png = os.path.join(args.outdir, f"surf_allviews__{suffix}.png")
    save_all_surf_views(surf_img=surf_img, fsavg=fsavg, fsavg_sulcal=fsavg_sulc, stat_title=args.title,
        out_png=surf_all_png, t_thresh=args.t_thresh, black_bg=args.black_bg,
    )

    
    # Glass brain (lyrz)
    display_glass = plot_glass_brain(stat_in, display_mode="lyrz",
        black_bg=args.black_bg, plot_abs=True, title=args.title,
        threshold=args.t_thresh if args.t_thresh is not None else 1,
    )  # returns a display object[web:1][web:13][web:14]
    glass_png = os.path.join(args.outdir, f"glass_lyrz__{suffix}.png")
    display_glass.savefig(glass_png, dpi=args.dpi)
    display_glass.close()

    # Stat on surface (both hemispheres, multiple views)
    display_surf_views = plot_img_on_surf(stat_map=stat_in, hemispheres=["left", "right"],
        views=["lateral", "medial", "dorsal"], title=args.title, bg_on_data=True,
    )  # returns (fig, axes) or nothing if output_file given; current API returns fig, axes[web:44]
    # if it returns (fig, axes):
    if isinstance(display_surf_views, tuple):
        fig_sv, _axes = display_surf_views
        if args.black_bg:
           fig_sv.patch.set_facecolor("black")
        else:
           fig_sv.patch.set_facecolor("white")
           
        surf_views_png = os.path.join(args.outdir, f"surf_views__{suffix}.png")
        fig_sv.savefig(surf_views_png, dpi=args.dpi)
        plt.close(fig_sv)
    else:
        # older/variant API: display-like object
        surf_views_png = os.path.join(args.outdir, f"surf_views__{suffix}.png")
        display_surf_views.savefig(surf_views_png, dpi=args.dpi)
        display_surf_views.close()

    # Contours on glass brain
    display_contours = plot_glass_brain(
        None, plot_abs=False, display_mode="lzry", black_bg=True
    )  # display object with savefig/close[web:39]
    display_contours.add_contours(stat_in,
        levels=[0] if args.t_thresh is None else [-args.t_thresh, args.t_thresh],
        colors=["b", "r"], linewidths=2.0,
    )
    display_contours.title(f"{args.title} (contours)")
    contours_png   = os.path.join(args.outdir, f"glass_contours__{suffix}.png")
    display_contours.savefig(contours_png, dpi=args.dpi)
    display_contours.close()
    # no plotting.show() for CLI

if __name__ == "__main__":
    main()

## example usage in bash
# python /wdata/msmuhammad/workbench/customized-functions/nilearn-viz-function__final.py \
#     --t_img spmT_0003_lang_specific_pos.nii \
#     --p_img p_uncorr_0003_lang_specific_pos.nii \
#     --p_thresh 0.005 \
#     --plot_mode full \
#     --t_thresh 3.0 \
#     --surf_hemi left \
#     --surf_view lateral \
#     --title "language-specific PC1" \
#     --outdir ${dd} \
#     --black_bg \
#     --stat_label "lang_PC1"
    

