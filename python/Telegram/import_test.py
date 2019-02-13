from telethon import TelegramClient
from telethon.tl.types import InputPeerChat
from telethon.tl.functions.messages import GetHistoryRequest
import csv

api_id = 92095
api_hash = 'e33fd27a8b4fd13829cd5e9e6444d562'
phone = '+4746397700'

client = TelegramClient('session_name',api_id, api_hash)
client.connect()

if not client.is_user_authorized():
    client.sign_in(phone=phone)
    client.sign_in(phone,input('Enter the code: '))

channel = client.get_entity('t.me/joinchat/CWuF8EA8ZNIn60hzbU4low')

zh_full = client(GetHistoryRequest(
    peer = channel,
    offset_id = 0,
    offset_date = None,
    add_offset = 0,
    limit = 20000,
    max_id = 0,
    min_id = 0
))

output=[]
for m in zh_full.messages:
    if hasattr(m, 'message'):
        row = (m.from_id, client.get_entity(m.from_id).username, str(m.message))
        output.append(row)

with open('zh_chat.csv', 'w', newline='', encoding='utf-8') as f:
    writer = csv.writer(f,delimiter='\t')
    writer.writerows(output)

