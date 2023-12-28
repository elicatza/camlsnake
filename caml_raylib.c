#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <raylib.h>
#include <sys/random.h>


Color color_of_value(value val)
{
    return (Color) {
        .r = Int_val(Field(val, 0)),
        .g = Int_val(Field(val, 1)),
        .b = Int_val(Field(val, 2)),
        .a = Int_val(Field(val, 3)),
    };
}

Vector2 vector_of_value(value val)
{
    return (Vector2) {
        .x = Double_field(val, 0),
        .y = Double_field(val, 1),
    };
}

CAMLprim value caml_unix_getentropy(value unit)
{
    CAMLparam1(unit);
    int seed;
    getentropy(&seed, sizeof seed);
    CAMLreturn(Val_int(seed));
}

CAMLprim value caml_init_window(value width, value height, value title)
{
    CAMLparam3(width, height, title);
    InitWindow(Int_val(width), Int_val(height), String_val(title));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_close_window(value unit)
{
    CAMLparam1(unit);
    CloseWindow();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_begin_drawing(value unit)
{
    CAMLparam1(unit);
    BeginDrawing();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_end_drawing(value unit)
{
    CAMLparam1(unit);
    EndDrawing();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_window_should_close(value unit)
{
    CAMLparam1(unit);
    bool rv = WindowShouldClose();
    CAMLreturn(Val_bool(rv));
}

CAMLprim value caml_clear_background(value color)
{
    CAMLparam1(color);
    ClearBackground(color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_draw_circle(value center, value radius, value color)
{
    CAMLparam3(center, radius, color);
    DrawCircleV(vector_of_value(center), Double_val(radius), color_of_value(color));
    CAMLreturn(Val_unit);
}
CAMLprim value caml_draw_rectangle(value pos_x, value pos_y, value width, value height, value color)
{
    CAMLparam5(pos_x, pos_y, width, height, color);
    DrawRectangle(Int_val(pos_x), Int_val(pos_y), Int_val(width), Int_val(height), color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_draw_line(value start, value end, value thick, value color)
{
    CAMLparam4(start, end, thick, color);
    DrawLineEx(vector_of_value(start),
               vector_of_value(end),
               Double_val(thick),
               color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_set_target_fps(value fps)
{
    CAMLparam1(fps);
    SetTargetFPS(Int_val(fps));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_is_key_pressed(value key)
{
    CAMLparam1(key);
    bool rv = IsKeyPressed(Int_val(key));
    CAMLreturn(Val_bool(rv));
}
