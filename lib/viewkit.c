// 使用するには/ProjectRoot/../ViewKit/にViewKitをクローンしてリリースビルドしてください。
// ViewKitはhttps://github.com/mochiOS/ViewKitから入手できます

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#include <viewkit.h>

static VkString ruca_vk_string(const char *value) {
    if (value == NULL) {
        return vk_string("", 0);
    }

    return vk_string(value, strlen(value));
}

VkRuntime *ruca_vk_runtime_create(int64_t component_instance_id) {
    return vk_runtime_create((uint64_t)component_instance_id);
}

int64_t ruca_vk_runtime_destroy(VkRuntime *runtime) {
    return (int64_t)vk_runtime_destroy(runtime);
}

int64_t ruca_vk_tree_begin(
    VkRuntime *runtime,
    int64_t root_node_id
) {
    return (int64_t)vk_tree_begin(
        runtime,
        (uint64_t)root_node_id
    );
}

int64_t ruca_vk_push_text(
    VkRuntime *runtime,
    int64_t node_id,
    const char *content
) {
    return (int64_t)vk_push_text(
        runtime,
        (uint64_t)node_id,
        ruca_vk_string(content),
        18.0f,
        27.0f,
        400,
        VK_TEXT_ALIGNMENT_START,
        VK_TEXT_COLOR_BLACK
    );
}

int64_t ruca_vk_tree_commit(VkRuntime *runtime) {
    return (int64_t)vk_tree_commit(runtime);
}

int64_t ruca_vk_runtime_run_window(
    VkRuntime *runtime,
    const char *title,
    double width,
    double height,
    int64_t resizable
) {
    return (int64_t)vk_runtime_run_window(
        runtime,
        ruca_vk_string(title),
        (float)width,
        (float)height,
        resizable != 0
    );
}

static int64_t ruca_vk_status(int32_t status) {
    return (int64_t)status;
}

int64_t ruca_vk_push_card(
    VkRuntime *runtime,
    int64_t node_id,
    const char *title,
    const char *body
) {
    VkRectangleStyle style = vk_rectangle_style_default();

    style.color_kind = VK_RECTANGLE_COLOR_SURFACE;
    style.radius_kind = VK_CORNER_RADIUS_CARD;
    style.border_kind = VK_BORDER_STANDARD;
    style.border_width = 1.0f;

    int32_t status;

    status = vk_begin_background(
        runtime,
        (uint64_t)node_id,
        style
    );

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_begin_padding(
        runtime,
        (uint64_t)(node_id + 1),
        20.0f,
        20.0f,
        20.0f,
        20.0f
    );

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_begin_vstack(
        runtime,
        (uint64_t)(node_id + 2),
        VK_STACK_GAP_SMALL,
        VK_ALIGNMENT_START,
        VK_DISTRIBUTION_START
    );

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_push_text(
        runtime,
        (uint64_t)(node_id + 3),
        ruca_vk_string(title),
        22.0f,
        30.0f,
        700,
        VK_TEXT_ALIGNMENT_START,
        VK_TEXT_COLOR_BLACK
    );

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_push_text(
        runtime,
        (uint64_t)(node_id + 4),
        ruca_vk_string(body),
        15.0f,
        23.0f,
        400,
        VK_TEXT_ALIGNMENT_START,
        VK_TEXT_COLOR_BLACK
    );

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_end_node(runtime);

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_end_node(runtime);

    if (status != VK_STATUS_OK) {
        return ruca_vk_status(status);
    }

    status = vk_end_node(runtime);

    return ruca_vk_status(status);
}
