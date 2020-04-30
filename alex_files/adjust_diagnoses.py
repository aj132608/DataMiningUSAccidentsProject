import pandas as pd
# import numpy as np
# import matplotlib.pyplot as plt


def determine_diag_map(icd9_code):
    """
    This function determines the diagnostic map
    depending on the table above. It Assigns a
    category referred by the group name in lower case
    :param icd9_code: String
    """
    if icd9_code.find("E") != -1 or \
            icd9_code.find("V") != -1:
        return "other"
    elif icd9_code.find("?") != -1:
        return None
    else:
        icd9_code = float(icd9_code)
        if 390 <= icd9_code <= 459 or \
                icd9_code == 785:
            return "circulatory"
        elif 460 <= icd9_code <= 519 or \
                icd9_code == 786:
            return "respiratory"
        elif 520 <= icd9_code <= 579 or \
                icd9_code == 787:
            return "digestive"
        elif 250 <= icd9_code < 251 or \
                icd9_code == 787:
            return "diabetes"
        elif 800 <= icd9_code <= 999:
            return "injury"
        elif 710 <= icd9_code <= 739:
            return "musculoskeletal"
        elif 580 <= icd9_code <= 629 or \
                icd9_code == 788:
            return "genitourinary"
        elif 140 <= icd9_code <= 239:
            return "neoplasms"
        else:
            return "other"


if __name__ == "__main__":
    diabetes_data = pd.read_csv('diabetic_data.csv')

    # print(diabetes_data.head())

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

    for diagnosis_type in possible_diagnosis:
        diabetes_data[diagnosis_type] = ""
    # Bin the diagnoses
    diabetes_data["diag_1_binned"] = list(map(determine_diag_map, diabetes_data["diag_1"].values))
    diabetes_data["diag_2_binned"] = list(map(determine_diag_map, diabetes_data["diag_2"].values))
    diabetes_data["diag_3_binned"] = list(map(determine_diag_map, diabetes_data["diag_3"].values))

    # Create a new data frame using the binned diagnoses
    diagnosis_df = diabetes_data[["diag_1_binned",
                                  "diag_2_binned",
                                  "diag_3_binned"]].stack().str.get_dummies().sum(level=0)

    diagnosis_df[diagnosis_df > 1] = 1

    for diagnosis_type in possible_diagnosis:
        diabetes_data[diagnosis_type] = diagnosis_df[diagnosis_type]

    print(diabetes_data.head())

    # for diagnosis_type in possible_diagnosis:
    #     print(diabetes_data[diagnosis_type].head())
    diabetes_data.to_csv('updated_data.csv')


