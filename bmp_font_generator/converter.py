from PIL import Image, ImageDraw, ImageFont

# --- ПАРАМЕТРЫ ---

char_width = 28     # ширина символа в пикселях
char_height = 32    # высота символа в пикселях
font_size = 28      # размер шрифта в pt (обычно чуть меньше высоты символа)

font_path = "arial.ttf"  # путь до TTF-файла
encoding = 'cp1251'      # кодировка символов

text_color = (0, 0, 0)             # цвет текста в RGB
background_color = (255, 255, 255) # цвет фона

output_file = "font_cp1251.bmp"

# --- ГЕНЕРАЦИЯ СИМВОЛОВ ---

# Генерация всех символов в кодировке Windows-1251
chars = ''.join(chr(i) for i in range(256)).encode('latin1').decode(encoding, errors='replace')

# Размер итогового изображения
img_width = len(chars) * char_width
img_height = char_height

# Создание изображения
img = Image.new('RGB', (img_width, img_height), color=background_color)
draw = ImageDraw.Draw(img)

# Загрузка шрифта
font = ImageFont.truetype(font_path, size=font_size)

# Отрисовка
for idx, char in enumerate(chars):
    x = idx * char_width
    y = 0   # по желанию можно скорректировать вертикальный отступ
    draw.text((x + 2, y), char, font=font, fill=text_color)

# Сохранение
img.save(output_file)

