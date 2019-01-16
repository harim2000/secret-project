select
    datetime(substr(date, 1, 9) + 978307200, 'unixepoch', 'localtime') as f_date,
    text,
	is_from_me,
	handle_id,
	payload_data
from message