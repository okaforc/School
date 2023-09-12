import json

t_full_data = json.load(open("Tumblr/payload-0.json"))
t_data = t_full_data[0]['data']

t_dash:list = t_data['client_side_ad_analytics']
t_dash = sorted(t_dash, key=lambda d: d["serve_time"]) # sort data based on serve time

# c_ad = 0
# c_post = 0
# c_other = 0
c_v = 0
c_i = 0
c_b = 0
ads = []
# # for i in t_dash:
for i in t_dash:
#     # print(i)
#     print(i["platform"], i["serve_time"])
#     match i["element_type"]:
#         case "post":
#             c_post += 1
#             if "http" not in i["content_url"]: print("post hmm")
#         case "advertisement":
#             c_ad += 1
#             # if "http" in i["content_url"]: print("ad hmm")
#             # print(i["content_url"])
#             pass
#         case "other":
#             c_other += 1
#             # print(i["content_url"])
#         case _:
#             print(i["element_type"])
# print(c_ad, c_post, c_other)

    # if "house-ad.tumblr.com" in i["post_url"]:
    #     ads.append(i["post_url"])
    
    if i["viewed"] == "true" and i["interacted"] == "true":
        c_b += 1
    else:
        if["viewed"] == "true":
            c_v += 1
        if i["interacted"] == "true":
            c_i += 1
    
    ads.append(i["ad_type"])
    if i["client_side_placement_id"] != "": print(i["client_side_placement_id"])
    pass

print(len(ads))
ads = list(set(ads))
print(len(ads))
[print(ad) for ad in ads]

print(c_i, c_v, c_b)
with open ('new_data.json', 'w') as f:
    f.write(json.dumps(t_dash, indent=4)) # write to a json file
 