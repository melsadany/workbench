import cv2
import dlib
import csv
import os
import argparse
from glob import glob

# Argument parsing
parser = argparse.ArgumentParser(description='Extract facial landmarks from an image or a directory of images.')
parser.add_argument('--image_path', type=str, help='Path to the input image file.')
parser.add_argument('--dir_path', type=str, help='Path to the directory containing image files.')
args = parser.parse_args()

# Initialize dlib's face detector and facial landmark predictor
detector = dlib.get_frontal_face_detector()
predictor_path = '/wdata/jmichaelson/face/dlib/shape_predictor_68_face_landmarks.dat'  # Update this path as needed
predictor = dlib.shape_predictor(predictor_path)

import os

# Global variables for landmark adjustment
selected_point = None
points = []

# Mouse callback function for OpenCV window
def select_point(event, x, y, flags, param):
    global selected_point, points
    print(f"Mouse event: {event}, Position: ({x}, {y})")  # Debugging line
    if event == cv2.EVENT_LBUTTONDOWN:
        min_distance = float("inf")
        for i, point in enumerate(points):
            distance = (x - point[0]) ** 2 + (y - point[1]) ** 2
            if distance < min_distance:
                min_distance = distance
                selected_point = i
    elif event == cv2.EVENT_LBUTTONUP and selected_point is not None:
        points[selected_point] = (x, y)
        selected_point = None

# Your process_image function, modified
def process_image(image_path, writer, show_image=True):
    global points
    manual_correction = False
    qc_pass = 'N'
    image = cv2.imread(image_path)
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    faces = detector(gray)

    for face in faces:
        landmarks = predictor(gray, face)
        points = [(point.x, point.y) for point in landmarks.parts()]

        while True:
            img_display = image.copy()
            for i, (x, y) in enumerate(points):
                color = (0, 0, 255) if i == selected_point else (0, 255, 0)
                cv2.circle(img_display, (x, y), 2, color, -1)

            if show_image:
                cv2.imshow('Facial Landmarks', img_display)

            key = cv2.waitKey(1)

            if key == 109:  # ASCII for 'm'
                manual_correction = True
                cv2.setMouseCallback('Facial Landmarks', select_point)
            elif key == 121:  # ASCII for 'y'
                qc_pass = 'M' if manual_correction else 'Y'
                break
            elif key == 110:  # ASCII for 'n'
                qc_pass = 'N'
                break

        cv2.setMouseCallback('Facial Landmarks', lambda *args: None)  # Remove mouse callback
        cv2.destroyAllWindows()
        if not os.path.exists("landmarked"):
            os.makedirs("landmarked")
        save_filename = f"{os.path.basename(image_path).split('.')[0]}_{qc_pass}_landmarked.png"
        save_path = os.path.join("landmarked", save_filename)
        cv2.imwrite(save_path, img_display)

        nose_tip_x = points[33][0]
        nose_tip_y = points[33][1]
        relative_coords = [(x - nose_tip_x, y - nose_tip_y) for (x, y) in points]
        row_data = [os.path.basename(image_path), nose_tip_x, nose_tip_y, qc_pass] + [coord for point in relative_coords for coord in point]
        writer.writerow(row_data)

if args.image_path:
    csv_file = os.path.basename(args.image_path).split('.')[0] + '_coords.csv'
    with open(csv_file, mode='w', newline='') as file:
        writer = csv.writer(file)
        header = ['filename', 'nose_tip_x', 'nose_tip_y', 'QCPASS']
        for i in range(68):
            header.extend([f'rel_x_{i}', f'rel_y_{i}'])
        writer.writerow(header)
        file.flush()  # Explicitly flush file buffer
        print(f"Processing image: {args.image_path}")
        cv2.namedWindow('Facial Landmarks')  # Moved outside the function
        cv2.setMouseCallback('Facial Landmarks', select_point)  # Initialize callback
        process_image(args.image_path, writer)
        file.flush()  # Explicitly flush file buffer

elif args.dir_path:
    csv_file = os.path.basename(args.dir_path.rstrip('/')) + '_coords.csv'
    with open(csv_file, mode='w', newline='') as file:
        writer = csv.writer(file)
        header = ['filename', 'nose_tip_x', 'nose_tip_y', 'QCPASS']
        for i in range(68):
            header.extend([f'rel_x_{i}', f'rel_y_{i}'])
        writer.writerow(header)
        file.flush()  # Explicitly flush file buffer
        cv2.namedWindow('Facial Landmarks')  # Moved outside the function
        cv2.setMouseCallback('Facial Landmarks', select_point)  # Initialize callback
        image_files = glob(f"{args.dir_path}/*.png") + glob(f"{args.dir_path}/*.jpg") + glob(f"{args.dir_path}/*.jpeg")
        for idx, image_file in enumerate(image_files):
            print(f"Processing image {idx + 1}/{len(image_files)}: {image_file}")
            process_image(image_file, writer)
            file.flush()  # Explicitly flush file buffer

else:
    print("Either --image_path or --dir_path must be specified.")

