def jmes: [leaf_paths] | map(map(. as $name | try (tonumber | "[]") catch $name) | join(".")) | unique;
def leaves: if type == "array" or type == "object" then .[] | leaves else . end;
