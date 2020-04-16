import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

diabetes_data = pd.read_csv('diabetic_data.csv')

# Extract the different values for the age attribute
ages = diabetes_data['age']
age_categories = np.unique(np.array(ages))

# Create the new categories
# [0,30)
age_0 = age_categories[:3]
# [30,60)
age_1 = age_categories[3:6]
# [60,100]
age_2 = age_categories[6:]
new_ages = []
age_occurrences = [0, 0, 0]

print(f'First age category: {age_0}')
print(f'Second age category: {age_1}')
print(f'Third age category: {age_2}')

# Create a new list replacing the old age intervals with the new ones
for age in ages.values:
    if age in age_0:
        new_ages.append(0)
        age_occurrences[0] += 1
    elif age in age_1:
        new_ages.append(1)
        age_occurrences[1] += 1
    elif age in age_2:
        new_ages.append(2)
        age_occurrences[2] += 1
    else:
        new_ages.append('NaN')

# Add the new category values to the DataFrame
new_age_series = pd.Series(new_ages)
diabetes_data['new_ages'] = new_age_series

new_age_category = np.unique(np.array(new_age_series))

print(f'New age categories: {new_age_category}')
print(f'Occurrences of each age category: {age_occurrences}')

# Plot the occurrences of each category
plt.bar(new_age_category, age_occurrences)
plt.show()
