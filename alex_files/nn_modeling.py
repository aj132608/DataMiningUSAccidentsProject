import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":
    possible_diagnosis = [
        "circulatory",
        "respiratory",
        "digestive",
        "diabetes",
        "injury",
        "musculoskeletal",
        "genitourinary",
        "neoplasms",
        "other"
    ]

    dataset_df = pd.read_csv('updated_data_removed_outliers.csv')

    feature_medications = dataset_df.iloc[:, 27:50]
    print(feature_medications.columns)
    for label in feature_medications.columns:

    # for index, label in enumerate(dataset_df.columns):
    #     print(f"[{index}] {label}")
