import json

def fix_emoji(text):
    initial_string = text
    final_string = initial_string.encode('ISO-8859-1').decode('UTF-8')
    return final_string

def fix_lines_in_json_file(input_file_path, output_file_path):
    with open(input_file_path, 'r', encoding='utf-8') as file:
        data = json.load(file)

    # Apply the fix_emoji function to each value in the JSON data
    fixed_data = recursive_fix(data, fix_emoji)

    with open(output_file_path, 'w', encoding='utf-8') as file:
        json.dump(fixed_data, file, ensure_ascii=False, indent=2)

def recursive_fix(data, fix_function):
    if isinstance(data, dict):
        return {key: recursive_fix(value, fix_function) for key, value in data.items()}
    elif isinstance(data, list):
        return [recursive_fix(item, fix_function) for item in data]
    elif isinstance(data, str):
        return fix_function(data)
    else:
        return data

# Example usage:
input_filename = r'C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\brudne\\pshemobalicki_1.json'
output_filename = r'C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\znaki_zmienione'
fix_lines_in_json_file(input_filename, output_filename)