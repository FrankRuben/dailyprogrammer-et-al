// -*- coding: utf-8 -*-

#include <ctype.h>
#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *lower[] = {
  u8"áăắặằẳẵǎâấậầẩẫäạàảāąåǻãɑɐɒ",
  u8"ḅɓß♭␢Б",
  u8"ćčçĉɕċ",
  u8"ďḓḍɗḏđɖ",
  u8"éĕěêếệềểễëėẹèẻēęẽɘəɚ",
  u8"ƒſʃʆʅɟʄ",
  u8"ǵğǧģĝġɠḡɡ",
  u8"ḫĥḥɦẖħɧ",
  u8"íĭǐîïịìỉīįɨĩɩı",
  u8"ǰĵʝȷɟʄ",
  u8"ķḳƙḵĸʞ",
  u8"ĺƚɬľļḽḷḹḻŀɫɭł",
  u8"ḿṁṃɱɯɰ",
  u8"ŉńňņṋṅṇǹɲṉɳñŋ",
  u8"óŏǒôốộồổỗöọőòỏơớợờởỡōǫøǿõɵʘ",
  u8"ɸþᵱƥᵽṗṕ",
  u8"ʠꝗɋq̃ϙ",
  u8"ŕřŗṙṛṝɾṟɼɽɿɹɻ",
  u8"śšşŝșṡṣʂ",
  u8"ťţṱțẗṭṯʈŧ",
  u8"ʉúŭǔûüǘǚǜǖụűùủưứựừửữūųůũʊ",
  u8"ʋʌⱴṿṽ",
  u8"ẃŵẅẁʍ",
  u8"χẍẋⲭ",
  u8"ýŷÿẏỵỳƴỷȳỹʎ",
  u8"źžʑżẓẕʐƶ",
};

static char *upper[] = {
  u8"ÁĂẮẶẰẲẴǍÂẤẬẦẨẪÄẠÀẢĀĄÅǺÃ",
  u8"ḄƁᛒ𐌱ɃḂḆ฿β",
  u8"ĆČÇĈĊƆʗ",
  u8"ĎḒḌƊḎĐÐ",
  u8"ÉĔĚÊẾỆỀỂỄËĖẸÈẺĒĘẼƐ",
  u8"ƑḞ𐌅₣",
  u8"ǴĞǦĢĜĠḠʛ",
  u8"ḪĤḤĦ",
  u8"ÍĬǏÎÏİỊÌỈĪĮĨ",
  u8"ĴɈʝ",
  u8"ĶḲƘḴ",
  u8"ĹȽĽĻḼḶḸḺĿŁ",
  u8"ḾṀṂ",
  u8"ŃŇŅṊṄṆǸƝṈÑ",
  u8"ÓŎǑÔỐỘỒỔỖÖỌŐÒỎƠỚỢỜỞỠŌƟǪØǾÕ",
  u8"Þ𐌐ṔṖⱣƤ₱♇",
  u8"ꝖɊ",
  u8"ŔŘŖṘṚṜṞʁ",
  u8"ŚŠŞŜȘṠṢ",
  u8"ŤŢṰȚṬṮŦ",
  u8"ÚŬǓÛÜǗǙǛǕỤŰÙỦƯỨỰỪỬỮŪŲŮŨ",
  u8"ṼṾƲ℣∨",
  u8"ẂŴẄẀʬ",
  u8"χẌẊⲬ𐍇",
  u8"ÝŶŸẎỴỲƳỶȲỸ",
  u8"ŹŽŻẒẔƵ",
};

int main () {
  setlocale(LC_ALL, "en_US.UTF-8");

  const char *input = "For, after all, how do we know that two and two make four? "
    "Or that the force of gravity works? Or that the past is unchangeable? "
    "If both the past and the external world exist only in the mind, "
    "and if the mind itself is controllable – what then?";

  const size_t n_output = strlen(input) * MB_CUR_MAX + 1; // why worry about a few more bytes...
  char output[n_output];
  char *output_ptr = output;

  const size_t n_chars = sizeof(lower) / sizeof(lower[0]);
  char *lower_ptr[n_chars];
  char *upper_ptr[n_chars];
  for (size_t i = 0; i < n_chars; i++) {
    lower_ptr[i] = lower[i];
    upper_ptr[i] = upper[i];
  }

  for (const char *input_ptr = input; *input_ptr; input_ptr++) {
    if (islower(*input_ptr)) {
      const size_t i = *input_ptr - 'a';
      if (*lower_ptr[i] == '\0') {
        lower_ptr[i] = lower[i];
      }
      const int l = mblen(lower_ptr[i], MB_CUR_MAX);
      for (size_t c = 0; c < l; c++) {
        *output_ptr++ = *(lower_ptr[i])++;
      }
    } else if (isupper(*input_ptr)) {
      const size_t i = *input_ptr - 'A';
      if (*upper_ptr[i] == '\0') {
        upper_ptr[i] = upper[i];
      }
      const int l = mblen(upper_ptr[i], MB_CUR_MAX);
      for (size_t c = 0; c < l; c++) {
        *output_ptr++ = *(upper_ptr[i])++;
      }
    } else {
      *output_ptr++ = *input_ptr;
    }
  }
  *output_ptr = '\0';
  printf("%s\n%s", input, output);

  return 0;
}

/*
 * See: https://www.reddit.com/r/dailyprogrammer/comments/4qg2eo/20160629_challenge_273_intermediate_twist_up_a/
 *
 * Output:

For, after all, how do we know that two and two make four? Or that the force of gravity works? Or that the past is unchangeable? If both the past and the external world exist only in the mind, and if the mind itself is controllable – what then?
Ƒóŕ, áƒťéř ăĺƚ, ḫŏẃ ďǒ ŵĕ ķŉôẅ ţĥắṱ țẁố ặńḓ ẗʍộ ḿằḳě ſồʉŗ? Óṙ ṭḥẳṯ ʈɦê ʃổṛćế ỗʆ ǵṝẵʋíŧý ẃöɾƙś? Ŏṟ ťẖǎţ ṱħệ ɸâšț ĭş úňčɧấņğềậḅɬể? Íʅ ɓọẗḫ ṭĥễ þầŝṯ ẩṋḍ ʈḥë ėχŧẹɼṅẫľ ŵőɽļɗ èẍǐșť òṇḽŷ îǹ ţɦẻ ṁïɲḏ, äṉđ ịɟ ṱẖē ṃìɳɖ ỉțṡęḷʄ īṣ çỏñẗɿơḹḻạßŀẽ – ẅħàṭ ṯɧɘŋ?

 */

/*
 * Local Variables:
 * compile-command: "gcc -Wall -Werror -std=c11 -O2 main.c -o main && ./main"
 * End:
 */
