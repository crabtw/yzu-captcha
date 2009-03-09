require 'RMagick'

module YzuCaptcha
  GRAY, BROWN, WHITE, BLACK, *FILLED_COLORS = [
    '#C0C0C0', '#8B4513', 'white', 'black',
    '#FF0000', '#00FF00', '#0000FF',
    '#FFFF00', '#00FFFF', '#FF00FF'
  ].map do |n|
    Magick::Pixel.from_color(n)
  end

  ImgWidth, ImgHeight = 16, 12

  module_function

  def segment(img)
    bin = to_bin(img)
    info = fill_color(BLACK, bin)
    new_info = locate_chars(info)
    chars = split(new_info, bin)

    return chars
  end

  def is_char_color(px)
      #luma = (
      #  px.red.to_f * 299 / 1000 +
      #  px.green.to_f * 587 / 1000 +
      #  px.blue.to_f * 114 / 1000
      #).floor

      px != GRAY && px != BROWN && px != WHITE &&
      px.opacity == 0 #&& luma < 0xC8C8
  end

  def check_bounds(width, height, x, y)
    return x >= 0 && x < width && y >= 0 && y < height
  end

  def to_bin(img)
    bin = Magick::Image.new(img.columns, img.rows)

    (0...img.rows).each do |y|
      (0...img.columns).each do |x|
        px, *sides = [
          [x, y], [x-1, y], [x+1, y], [x, y-1], [x, y+1]
        ].select do |pos_x, pos_y|
          check_bounds(img.columns, img.rows, pos_x, pos_y)
        end.map do |pos_x, pos_y|
          img.pixel_color(pos_x, pos_y)
        end

        color = 
        if is_char_color(px)
          BLACK
        elsif px.opacity != 0
          if sides.combination(2).any? {|a, b|
               is_char_color(a) && is_char_color(b)
             }
            BLACK
          else
            WHITE
          end
        else
          WHITE
        end

        bin.pixel_color(x, y, color)
      end
    end

    return bin
  end

  def flood(x, y, target, filled, img)
    left = right = x
    top = bottom = y

    if check_bounds(img.columns, img.rows, x, y)
      if img.pixel_color(x, y) == target
        img.pixel_color(x, y, filled)

        [
          [x-1, y], [x+1, y],
          [x, y-1], [x, y+1],
          [x-1, y-1], [x+1, y-1],
          [x-1, y+1], [x+1, y+1]
        ].each do |pos_x, pos_y|
          new_left, new_right, new_top, new_bottom =
          flood(pos_x, pos_y, target, filled, img)

          if new_left < left then left = new_left end
          if new_right > right then right = new_right end
          if new_top < top then top = new_top end
          if new_bottom > bottom then bottom = new_bottom end
        end
      end
    end

    return [left, right, top, bottom]
  end

  def fill_color(target, img)
    horizons = [7, 12, 16]
    num = 0
    info = []

    horizons.each do |y|
      (0...img.columns).each do |x|
        if img.pixel_color(x, y) == target
          left, right, top, bottom =
            flood(x, y, target, FILLED_COLORS[num], img)

          info << [
            [left+1, top+1, right-left-1, bottom-top-1],
            FILLED_COLORS[num]
          ]
          x = right
          num += 1
        else
          x += 1
        end
      end
    end

    return info.select do |edge, color|
      x, y, width, height = edge
      height >= ImgHeight / 2
    end
  end

  def div_position(num, char_info)
    x, y, width, height, color = char_info.flatten
    size = width / num
    new = []

    (1...num).each do
      new << [[x, y, size, height], color]
      x += size
      width -= size
    end
    new << [[x, y, width, height], color]
  end

  def locate_chars(info)
    return info if info.size >= 4

    num = 4 - info.length + 1
    w1, w2 = info.map do |edge, color|
      x, y, width, height = edge
      width
    end.sort {|i1, i2| i2 <=> i1}.take(2)

    if info.length == 3 || info.length == 1 || w1 - w2 > 10
      info.reduce([]) do |sum, ci|
        x, y, width, height = ci.first
        if w1 == width
          sum.concat(div_position(num, ci))
        else
          sum << ci
        end
      end
    else
      info.reduce([]) do |sum, ci|
        sum.concat(div_position(2, ci))
      end
    end
  end

  def split(info, img)
    info.map do |edge, color|
      x, y, char_width, char_height = edge

      if char_width > ImgWidth then char_width = ImgWidth end
      if char_height > ImgHeight then char_height = ImgHeight end

      new_px = img.get_pixels(x, y, char_width, char_height).map do |px|
        if px == color then BLACK else WHITE end
      end

      Magick::Image.
        new(ImgWidth, ImgHeight).
        store_pixels(
          (ImgWidth-char_width)/2,
          (ImgHeight-char_height)/2,
          char_width, char_height, new_px
        )
    end
  end
end
