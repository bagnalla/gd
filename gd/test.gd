func upd_dict(dict, k, v):
	if dict.has(k):
		dict[k].append(v)
	else:
		dict[k] = [v]