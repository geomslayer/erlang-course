defmodule Steganography do
  def get_offset(data) do
    <<_ :: size(80), offset :: native-size(32), _ :: bits>> = data
    offset
  end

  def get_parts(data) do
    offset = get_offset(data)
    <<header :: bytes-size(offset), pixels :: bytes>> = data
    {header, pixels}
  end

  def save_bmp(path, header, pixels) do
    File.write!(path, <<header :: bits, pixels :: bits>>)
  end

  def combine(secret, storage, step) do
    case secret do
      <<cur :: bits-size(step), others_secret :: bits>> ->
        <<cur_byte :: bytes-size(1), others_storage :: bytes>> = storage
        prefix_size = 8 - step
        <<prefix :: bits-size(prefix_size), _ :: bits>> = cur_byte
        <<prefix :: bits, cur :: bits, combine(others_secret, others_storage, step) :: bytes>>
      "" -> ""
    end
  end

  def extract(cur_byte, step) do
    prefix_size = 8 - step
    <<prefix :: bits-size(prefix_size), suffix :: bits>> = cur_byte
    suffix
  end

  def hide("", pixels, step, count) do
    {pixels, count}
  end

  def hide(string, pixels, step, count) do
    first = String.first(string)
    byte_size(first)
    len = div (8 * byte_size(first)), step
    case pixels do
      <<bs :: bytes-size(len), other_pixels :: bytes>> ->
        {hided, count} = hide(String.slice(string, 1..-1), other_pixels, step, count + len)
        {<<combine(first, bs, step) :: bytes, hided :: bytes>>, count}
      _ -> {pixels, count}
    end
  end

  def show(pixels, step, 0) do
    ""
  end

  def show(pixels, step, count) do
    <<cur_byte :: bytes-size(1), others :: bytes>> = pixels
    <<extract(cur_byte, step) :: bits, show(others, step, count - 1) :: bits>>
  end

  def to_binary(number, 0) do
    ""
  end

  def to_binary(number, size) do
    <<to_binary(div(number, 256), size - 1) :: bits, rem(number, 256)>>
  end

  def to_number("", acc) do
    acc
  end

  def to_number(binary, acc \\ 0) do
    <<fst :: integer, others :: bytes>> = binary
    to_number(others, acc * 256 + fst)
  end

  def construct_meta(len) do
    to_binary(len, 4)
  end

  def encode(src, dest, string, step \\ 2) do
    # hides string in src file and saves to dest
    #
    # src - given bmp image path
    # dest - result bmp image path
    # string - message to hide
    # step - how many bits of every byte will be used for data storage,
    #        it can be 1, 2, 4 or 8

    data = File.read! src
    {header, pixels} = get_parts(data)
    for_count_size = div 4 * 8, step
    <<for_count :: bytes-size(for_count_size), for_secret :: bytes>> = pixels
    {hided, count} = hide(string, for_secret, step, 0)
    meta = combine(construct_meta(count), for_count, step)
    new_data = header <> meta <> hided
    File.write! dest, new_data
  end

  def decode(src, step \\ 2) do
    # extracts hidden text from src file
    #
    # src - path of bmp image with hidden message
    # step - how many bits of every byte was used for data storage,
    #        it could be 1, 2, 4 or 8
    #
    # returns extracted hidden message

    data = File.read! src
    {header, pixels} = get_parts(data)
    for_count_size = div 4 * 8, step
    <<for_count :: bytes-size(for_count_size), for_secret :: bytes>> = pixels
    count = to_number(show(for_count, step, for_count_size))
    show(for_secret, step, count)
  end
end

lena = "lena.bmp"
text = "«Ле́на» (англ. Lenna, нередко также Lena; швед. Lena) — название стандартного тестового " <>
       "изображения, широко используемого в научных работах для проверки и иллюстрации алгоритмов " <>
       "обработки изображений (сжатия, шумоподавления и т. д.)."

Steganography.encode(lena, "1_" <> lena, text, 1)
text_1 = Steganography.decode("1_" <> lena, 1)
IO.inspect text_1

Steganography.encode(lena, "4_" <> lena, text, 4)
text_4 = Steganography.decode("4_" <> lena, 4)
IO.inspect text_4

bird = "bird.bmp"
Steganography.encode(bird, "new_" <> bird, text)
text_bird = Steganography.decode("new_" <> bird)
IO.inspect text_bird